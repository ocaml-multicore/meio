module Queue = Eio_utils.Lf_queue

let table = Lwd_table.make ()
let waiting = Queue.create ()

let buf_fmt_key =
  Domain.DLS.new_key (fun () ->
      let buf = Buffer.create 2000 in
      (buf, Format.formatter_of_buffer buf))

let mtx = Mutex.create ()

let () =
  Logs.set_reporter_mutex
    ~lock:(fun () -> Mutex.lock mtx)
    ~unlock:(fun () -> Mutex.unlock mtx)

let reporter () =
  let report src level ~over k msgf =
    let buf, log_fmt = Domain.DLS.get buf_fmt_key in
    let k _ =
      Format.pp_print_flush log_fmt ();
      let msg = Buffer.contents buf in
      Buffer.clear buf;
      Queue.push waiting msg;
      over ();
      k ()
    in
    let log header tags k fmt = Format.kfprintf k log_fmt fmt in
    msgf @@ fun ?header ?tags fmt -> log header tags k fmt
  in
  { Logs.report }

let rec poll () =
  match Queue.pop waiting with
  | Some msg ->
      Lwd_table.prepend' table msg;
      poll ()
  | None -> ()

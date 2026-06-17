module Queue = Eio_utils.Lf_queue

let table = Lwd_table.make ()
let waiting = Queue.create ()

type 'a log =
  ((?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) ->
  unit

let stamp_tag : Mtime.span Logs.Tag.def =
  Logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.Span.pp

let c = Mtime_clock.counter ()
let stamp () = Logs.Tag.(empty |> add stamp_tag (Mtime_clock.count c))
let info fmt = Logs.info (fun f -> fmt @@ f ~tags:(stamp ()))
let warn fmt = Logs.warn (fun f -> fmt @@ f ~tags:(stamp ()))
let debug fmt = Logs.debug (fun f -> fmt @@ f ~tags:(stamp ()))

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
  let report _src level ~over k msgf =
    let buf, log_fmt = Domain.DLS.get buf_fmt_key in
    let k _ =
      Format.pp_print_flush log_fmt ();
      let msg = Buffer.contents buf in
      Fmt.epr "%s\n%!" msg;
      Buffer.clear buf;
      Queue.push waiting msg;
      over ();
      k ()
    in
    let with_stamp h tags k fmt =
      let stamp =
        match tags with
        | None -> None
        | Some tags -> Logs.Tag.find stamp_tag tags
      in
      let dt =
        match stamp with None -> 0L | Some s -> Mtime.Span.to_uint64_ns s
      in
      Format.kfprintf k log_fmt
        ("%a[%a] @[" ^^ fmt ^^ "@]")
        Logs.pp_header (level, h) Fmt.uint64_ns_span dt
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  { Logs.report }

let rec poll () =
  match Queue.pop waiting with
  | Some msg ->
      Lwd_table.prepend' table msg;
      poll ()
  | None -> ()

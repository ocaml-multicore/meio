let table = Lwd_table.make ()
let buf = Buffer.create 200
let log_fmt = Format.formatter_of_buffer buf

let reporter () =
  let report src level ~over k msgf =
    let k _ =
      Format.pp_print_flush log_fmt ();
      let msg = Buffer.contents buf in
      Buffer.clear buf;
      Lwd_table.append' table msg;
      over ();
      k ()
    in
    let log header tags k fmt = Format.kfprintf k log_fmt fmt in
    msgf @@ fun ?header ?tags fmt -> log header tags k fmt
  in
  { Logs.report }

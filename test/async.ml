open Eio

let main ?(n = 200) _env =
  Fiber.both
    (fun () -> for x = 1 to n do traceln "x = %d" x; Eio_unix.sleep 0.2 done)
    (fun () -> for y = 1 to n do traceln "y = %d" y; Eio_unix.sleep 0.2 done)

let () =
  Eio_unix.Ctf.with_tracing "trace.ctf" @@ fun () ->
  Eio_main.run @@ fun env ->
  main env
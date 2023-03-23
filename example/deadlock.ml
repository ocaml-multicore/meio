open Eio

let fork wait =
  Switch.run @@ fun sw ->
  Fiber.fork ~sw (fun () ->
      (* Also add a really big label to test the handling of that in CTF. *)
      Eio.Private.Ctf.log (String.make 5000 'e');
      Promise.await wait)

let main () =
  let p1, r1 = Promise.create () in
  fork p1;
  Promise.resolve r1 ()

let () = Eio_main.run @@ fun _ -> main ()

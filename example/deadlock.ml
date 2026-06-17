open Eio

let fork wait =
  Switch.run ~name:"deadlock" (fun sw ->
      Fiber.fork ~sw (fun () ->
          Eio.traceln "Inside deadlock fork...";
          (* Also add a really big label to test the handling of that in CTF. *)
          Eio_name.name (String.make 5000 'e');
          Promise.await wait))

let main () =
  let p1, r1 = Promise.create ~label:"deadlock" () in
  fork p1;
  Promise.resolve r1 ()

let () = Eio_main.run @@ fun _ -> main ()

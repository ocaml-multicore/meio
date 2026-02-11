open Eio

let spawn ~clock min max =
  Switch.run @@ fun sw ->
  for i = min to max do
    Fiber.fork ~sw (fun () -> Time.sleep clock (float_of_int i));
    Time.sleep clock (float_of_int (max - i))
  done

(* Based on the Tokio Console example application *)
let main clock =
  let p, r = Promise.create () in
  Switch.run @@ fun sw ->
  (* A long running task *)
  Fiber.fork ~sw (fun () ->
      traceln "Waiting";
      Promise.await p;
      traceln "Done");
  Fiber.both (fun () -> spawn ~clock 5 10) (fun () -> spawn ~clock 10 30);
  Promise.resolve r ()

let () =
  Eio_main.run @@ fun env ->
  let clock = Stdenv.clock env in
  main clock

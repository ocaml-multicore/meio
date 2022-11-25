open Eio

let woops_sleepy ~clock =
  Switch.run @@ fun sw ->
  Fiber.fork ~sw (fun () ->
      (* Woops! Wrong sleep function, we blocked the fiber *)
      traceln "Woops! Blocked by Unix.sleepf";
      Unix.sleepf 5.;
      Time.sleep clock 10.)

let spawn ~clock min max =
  Switch.run @@ fun sw ->
  for i = min to max do
    Fiber.fork ~sw (fun () ->
        for _i = 0 to max do
          Time.sleep clock 0.2;
          Fiber.yield ()
        done;
        Time.sleep clock (float_of_int i));
    Time.sleep clock (float_of_int (max - i))
  done

(* Based on the Tokio Console example application *)
let main clock =
  let p, r = Promise.create () in
  Switch.run @@ fun sw ->
  (* A long running task *)
  Fiber.fork ~sw (fun () ->
      traceln "stuck waiting :(";
      Promise.await p;
      traceln "Done");
  Fiber.all
    [
      (fun () -> spawn ~clock 5 10);
      (fun () -> spawn ~clock 10 30);
      (fun () -> woops_sleepy ~clock);
    ];
  Promise.resolve r ()

let () =
  Eio_main.run @@ fun env ->
  Ctf.with_tracing @@ fun () ->
  let clock = Stdenv.clock env in
  main clock

open Eio

let woops_sleepy ~clock =
  Switch.run ~name:"unix sleeper ctx" @@ fun sw ->
  Fiber.fork ~sw (fun () ->
      Eio_name.name "unix sleeper";
      (* Woops! Wrong sleep function, we blocked the fiber *)
      traceln "Woops! Blocked by Unix.sleepf";
      Unix.sleepf 5.;
      Time.sleep clock 10.)

let spawn ~clock min max =
  Eio_name.name (Fmt.str "spawn %d %d" min max);
  (* Some GC action *)
  for _i = 0 to 100 do
    ignore (Sys.opaque_identity @@ Array.init 1000000 float_of_int)
  done;
  Switch.run ~name:"spawn ctx" @@ fun sw ->
  for i = min to max do
    Fiber.fork ~sw (fun () ->
        Eio_name.name (Fmt.str "worker>%d" i);
        for i = 0 to max do
          (* Some more GC action *)
          for _i = 0 to 100 do
            ignore (Sys.opaque_identity @@ Array.init 1000000 float_of_int)
          done;
          traceln "working... %d / %d" i max;
          Time.sleep clock 0.2;
          Fiber.yield ()
        done;
        Time.sleep clock (float_of_int i));
    Time.sleep clock (float_of_int (max - i))
  done

(* Based on the Tokio Console example application *)
let main clock =
  let p, r = Promise.create () in
  Switch.run ~name:"main" @@ fun sw ->
  Eio_name.name "main fiber";
  (* A long running task *)
  Fiber.fork ~sw (fun () ->
      Eio_name.name "waiter";
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
  let clock = Stdenv.clock env in
  main clock

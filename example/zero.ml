open Eio

let main clock =
  Eio.Switch.run ~name:"main context" @@ fun sw ->
  (* Eio.Fiber.fork ~sw (fun () ->
      Eio.Time.sleep clock 1.0;
      traceln "this is a forked function";
      (* Eio.Switch.run ~name:"forked context" @@ fun sw ->
      traceln "in another context I do this" *)) *)
  (* for i = 0 to 10 do
       Eio.Switch.run ~name:(Fmt.str "switch %d" i) @@ fun sw ->
       traceln "welcome %d" i
     done; *)
  Eio.Time.sleep clock 100000.0

let () =
  Eio_main.run @@ fun env ->
  let clock = Stdenv.clock env in
  main clock

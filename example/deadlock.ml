open Eio

let fork wait =
  Switch.run @@ fun sw ->
  Fiber.fork ~sw (fun () -> Promise.await wait)

let main () =
  let p1, r1 = Promise.create () in
  fork p1; 
  Promise.resolve r1 ()

let () = Eio_main.run @@ fun _ -> main ()

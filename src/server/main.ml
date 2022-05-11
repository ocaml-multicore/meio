open Eio.Std
open Cohttp_eio
open Websocket

let loader path =
  match Static.read path with
  | None ->
      traceln "%s not found" path;
      Server.Response.not_found
  | Some asset ->
      let mime = Magic_mime.lookup path in
      let headers =
        Cohttp.Header.of_list
          [
            ("Content-Type", mime);
            ("Content-Length", string_of_int @@ String.length asset);
          ]
      in
      Server.Response.create ~headers (String asset)

let handle_client pid send = Rev.start_trace_record send pid

let handler ~sw view _pid : Cohttp_eio.Server.handler =
 fun request ->
  let open Frame in
  let uri = Cohttp_eio.Server.Request.resource request in
  match uri with
  | "/websocket" ->
      let resp, send_frame =
        Websocket_eio.upgrade_connection request (fun { opcode; content; _ } ->
            match opcode with
            | Opcode.Close -> traceln "[RECV] CLOSE"
            | _ -> traceln "[RECV] %s" content)
      in
      let go () =
        (* handle_client pid (fun content ->
            send_frame @@ Frame.create ~content ()); *) ()
      in
      Fiber.fork ~sw go;
      Fiber.fork ~sw (fun () -> send_frame @@ Frame.create ~content:(Rev.Events.to_json @@ Rev.Events.Eio view |> Ezjsonm.value_to_string) ());
      resp
  | "/" | "/index.html" -> loader "index.html"
  | path -> loader path

let start_server env sw view pid port =
  traceln "[SERV] listening on port %d" port;
  Cohttp_eio.Server.run ~port env sw (handler ~sw view pid)

open Cmdliner

let runtime_ev_file =
  let doc = "Runtime events file e.g. 9987.events." in
  Arg.(required @@ opt (some string) None @@ info ~doc ~docv:"EVENTS" [ "events" ])

let main events_file src =
  let pid =
    match String.split_on_char '.' events_file with
    | pid :: [ "events" ] ->
        print_endline pid;
        int_of_string pid
    | _ -> failwith "Bad eventring file"
  in
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw -> 
  let view = Mtv.load (List.hd src) in
  start_server env sw view (Some (".", pid)) 8080

let () =
  let doc = "view OCaml runtime events and Eio fibers" in
  let man = [] in
  let info = Cmd.info ~doc ~man "ec" in
  let term = Term.(const main $ runtime_ev_file $ Mtv_unix.trace_files) in
  exit @@ Cmd.eval (Cmd.v info term)
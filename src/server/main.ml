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

let handler ~sw pid : Cohttp_eio.Server.handler =
 fun request ->
  let open Frame in
  let uri = Cohttp_eio.Server.Request.resource request in
  match uri with
  | "/websocket" ->
      traceln "[PATH] /ws";
      let resp, send_frame =
        Websocket_eio.upgrade_connection request (fun { opcode; content; _ } ->
            match opcode with
            | Opcode.Close -> traceln "[RECV] CLOSE"
            | _ -> traceln "[RECV] %s" content)
      in
      let go () =
        handle_client pid (fun content ->
            send_frame @@ Frame.create ~content ())
      in
      Fiber.fork ~sw go;
      resp
  | "/" | "/index.html" -> loader "index.html"
  | path -> loader path

let start_server env sw pid port =
  traceln "[SERV] Listening for HTTP on port %d" port;
  Cohttp_eio.Server.run ~port env sw (handler ~sw pid)

let () =
  let pid =
    match String.split_on_char '.' Sys.argv.(1) with
    | pid :: [ "events" ] ->
        print_endline pid;
        int_of_string pid
    | _ -> failwith "Bad eventring file"
  in
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw -> start_server env sw (Some (".", pid)) 8080

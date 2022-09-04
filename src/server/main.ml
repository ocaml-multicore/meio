open Eio.Std
open Cohttp_eio
open Websocket

let loader path req =
  match Static.read path with
  | None ->
      traceln "%s not found" path;
      Server.not_found_handler req
  | Some asset ->
      let mime = Magic_mime.lookup path in
      let headers =
        Http.Header.of_list
          [
            ("Content-Type", mime);
            ("Content-Length", string_of_int @@ String.length asset);
          ]
      in
      (Http.Response.make ~headers (), Cohttp_eio.Body.Fixed asset)

let handle_client pid mgr send = Rev.start_trace_record mgr send pid

let handler ~sw ~mgr pid : Cohttp_eio.Server.handler =
 fun ((request, _body, _stream) as req) ->
  let open Frame in
  let uri = Http.Request.resource request in
  match uri with
  | "/websocket" ->
      let resp, send_frame =
        Websocket_eio.upgrade_connection req (fun { opcode; content; _ } ->
            match opcode with
            | Opcode.Close -> traceln "[RECV] CLOSE"
            | _ -> traceln "[RECV] %s" content)
      in
      let go () =
        handle_client pid mgr (fun content ->
            send_frame @@ Frame.create ~content ())
      in
      Fiber.fork ~sw go;
      resp
  | "/" | "/index.html" -> loader "index.html" req
  | path -> loader path req

let start_server env sw pid port =
  traceln "[SERV] listening on port %d" port;
  let mgr = Eio.Stdenv.domain_mgr env in
  Cohttp_eio.Server.run ~port env (handler ~sw ~mgr pid)

let () =
  let pid =
    match String.split_on_char '.' Sys.argv.(1) with
    | pid :: [ "events" ] -> int_of_string pid
    | _ -> failwith "Bad eventring file"
  in
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw -> start_server env sw (Some (".", pid)) 8080

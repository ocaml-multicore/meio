open Eio
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

let exec ~sw env exec_args port =
  let argsl = String.split_on_char ' ' exec_args in
  let executable_filename = List.hd argsl in
  let env_vars = [| "OCAML_RUNTIME_EVENTS_START=1" |] in
  let child_pid =
    Unix.create_process_env executable_filename (Array.of_list argsl) env_vars
      Unix.stdin Unix.stdout Unix.stderr
  in
  Time.sleep env#clock 0.1;
  start_server env sw (Some (".", child_pid)) port

open Cmdliner

let exec_args p =
  let doc =
    "Executable (and its arguments) to trace. If the executable takes \
     arguments, wrap quotes around the executable and its arguments. For \
     example, olly '<exec> <arg_1> <arg_2> ... <arg_n>'."
  in
  Arg.(required & pos p (some string) None & info [] ~docv:"EXECUTABLE" ~doc)

let port =
  Arg.value @@ Arg.opt Arg.int 8080
  @@ Arg.info ~doc:"The port to run the server on" ~docv:"PORT" [ "port" ]

let exec_cmd env sw =
  let man =
    [
      `S Manpage.s_description;
      `P "Execute a given command and run the server for it.";
    ]
  in
  let doc = "Monitor a program with a UI in the browser." in
  let info = Cmd.info "exec" ~doc ~man in
  Cmd.v info Term.(const (exec ~sw env) $ exec_args 0 $ port)

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let main_cmd =
    let doc = "A browser-based monitoring tool using Runtime Events" in
    let info = Cmd.info "ec" ~doc in
    Cmd.group info [ exec_cmd env sw ]
  in
  exit (Cmd.eval main_cmd)

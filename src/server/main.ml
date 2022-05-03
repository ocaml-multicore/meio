let loader _root path _request =
  match Static.read path with
  | None -> Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

let clients : (int, Dream.websocket) Hashtbl.t =
  Hashtbl.create 5

let track =
  let last_client_id = ref 0 in
  fun websocket ->
    last_client_id := !last_client_id + 1;
    Hashtbl.replace clients !last_client_id websocket;
    !last_client_id

let forget client_id =
  Hashtbl.remove clients client_id

let handle_client pid client =
  let open Lwt.Syntax in
  let _client_id = track client in
  let+ () = Rev.start_trace_record client pid in
  ()

let () =
  let pid = 
    match String.split_on_char '.' Sys.argv.(1) with
    | pid :: [ "events" ] -> print_endline pid; int_of_string pid
    | _ -> failwith "Bad eventring file"
  in
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (fun _ -> Dream.html Index.render);
    Dream.get "/websocket" (fun _ -> Dream.websocket (handle_client (Some (".", pid))));
    Dream.get "/**" (Dream.static ~loader "")
  ]
let exec = Filename.basename Sys.executable_name

let run exec_args =
  let argsl = String.split_on_char ' ' exec_args in
  let executable_filename = List.hd argsl in
  let argsl = Array.of_list argsl in
  let tmp_dir = Filename.get_temp_dir_name () in
  let env =
    Array.append
      [|
        "OCAML_RUNTIME_EVENTS_START=1";
        "OCAML_RUNTIME_EVENTS_DIR=" ^ tmp_dir;
        "OCAML_RUNTIME_EVENTS_PRESERVE=1";
      |]
      (Unix.environment ())
  in
  let dev_null =
    Unix.openfile Filename.null [ Unix.O_WRONLY; Unix.O_KEEPEXEC ] 0o666
  in
  let child_pid =
    Unix.create_process_env executable_filename argsl env Unix.stdin dev_null
      dev_null
  in
  Unix.sleepf 0.2;
  let handle = (tmp_dir, child_pid) in
  Meio.ui ~child_pid handle;
  Unix.close dev_null;
  Unix.kill child_pid Sys.sigkill;
  let ring_file =
    Filename.concat tmp_dir (string_of_int child_pid ^ ".events")
  in
  Unix.unlink ring_file

let main _env =
   let usage = Printf.sprintf
    "Usage: %s [EXECUTABLE]\n\
     \ Monitor Eio programs.\n\
     Options:" exec
  in
  let err_no_executable () =
    raise (Arg.Bad "specify a single executable to monitor")
  in
  let executable = ref None in
  let set_exec f =
    if !executable <> None then err_no_executable () else
    executable := Some f
  in
  Arg.parse [] set_exec usage;
  match !executable with
  | None -> err_no_executable ()
  | Some exec -> run exec

let () = Eio_main.run main 

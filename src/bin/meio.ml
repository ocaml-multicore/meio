type program = Pid of int | Args of string * string list

let run ?stdout_file prog =
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
  let stdout =
    match stdout_file with
    | None -> dev_null
    | Some f ->
        Unix.openfile f
          [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY; Unix.O_KEEPEXEC ]
          0o666
  in
  let start =
    Runtime_events.Timestamp.get_current () |> Runtime_events.Timestamp.to_int64
  in
  let child_pid =
    match prog with
    | Pid i -> i
    | Args (exec, args) ->
        let argsl = Array.of_list (exec :: args) in
        Unix.create_process_env exec argsl env dev_null stdout stdout
  in
  let handle = (tmp_dir, child_pid) in
  Meio.ui ~child_pid ~start handle;
  Unix.close dev_null;
  Option.iter (fun _ -> Unix.close stdout) stdout_file;
  let () =
    try Unix.kill child_pid Sys.sigkill
    with Unix.Unix_error (Unix.ESRCH, "kill", _) -> ()
  in
  let ring_file =
    Filename.concat tmp_dir (string_of_int child_pid ^ ".events")
  in
  Unix.unlink ring_file

open Cmdliner

let executable =
  let doc = "The executable to run and monitor." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"EXECUTABLE")

let stdout_file =
  let doc = "A file to write the monitored program's stdout to." in
  Arg.(
    value & opt (some string) None & info [ "stdout" ] ~doc ~docv:"STDOUT_FILE")

let args =
  let doc = "Arguments for the executable to monitor." in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARGS")

let run_cmd =
  let open Cmdliner.Term.Syntax in
  let doc = "Monitor Eio programs from the terminal" in
  let man =
    [
      `S Manpage.s_examples;
      `P
        "If your Eio program does not take arguments, or if all the \
         argumentsare positional, run commands with `$(cmd) <executable> \
         <arg1> <arg2> ...'";
      `P
        "If your Eio program needs flags then use `--' disambiguation, e.g., \
         `$(cmd) -- <executable> --flag1 --flag2=foo ...'";
      `S Manpage.s_bugs;
      `P "Send bug reports to https://github.com/tarides/meio.";
    ]
  in
  Cmd.make (Cmd.info "meio" ~version:"%%VERSION%%" ~doc ~man)
  @@
  let+ executable and+ stdout_file and+ args in
  run ?stdout_file (Args (executable, args))

let main () = Cmd.eval run_cmd
let () = if !Sys.interactive then () else exit (main ())

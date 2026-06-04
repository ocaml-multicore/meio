let run (exec, args) =
  let argsl = Array.of_list args in
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
    Unix.create_process_env exec argsl env Unix.stdin dev_null dev_null
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

open Cmdliner

let executable =
  let doc = "The executable to run and monitor." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"EXECUTABLE")

let args =
  let doc = "Arguments for the executable to monitor." in
  Arg.(value & pos_right 1 string [] & info [] ~doc ~docv:"ARGS")

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
  let+ executable and+ args in
  run (executable, args)

let main () = Cmd.eval run_cmd
let () = if !Sys.interactive then () else exit (main ())

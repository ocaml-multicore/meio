module Process = Eio_luv.Low_level.Process

let run _stdenv exec_args =
  let argsl = String.split_on_char ' ' exec_args in
  let executable_filename = List.hd argsl in
  let tmp_dir = Filename.get_temp_dir_name () in
  let pair = function x :: [ y ] -> (x, y) | _ -> assert false in
  let env =
    Array.append
      [|
        ("OCAML_RUNTIME_EVENTS_START", "1");
        ("OCAML_RUNTIME_EVENTS_DIR", tmp_dir);
        ("OCAML_RUNTIME_EVENTS_PRESERVE", "1");
      |]
      (Unix.environment ()
      |> Array.map (fun s -> String.split_on_char '=' s |> pair))
    |> Array.to_list
  in
  let child_pid = Process.spawn ~env executable_filename argsl in
  Unix.sleepf 0.2;
  let handle = (tmp_dir, Process.pid child_pid) in
  Meio.ui handle;
  let ring_file =
    Filename.concat tmp_dir (string_of_int (Process.pid child_pid) ^ ".events")
  in
  Unix.unlink ring_file

let () = Eio_luv.run @@ fun stdenv -> run stdenv Sys.argv.(1)

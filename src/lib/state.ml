(* Mutable State *)
let tasks = Task_tree.make ()

let set_parent ~child ~parent ts =
  Task_tree.set_parent tasks ~child:(Task.Id.eio_of_int child)
    ~parent:(Task.Id.eio_of_int parent)
    (Runtime_events.Timestamp.to_int64 ts)

let add_tasks ~id ~parent_id ~domain ts kind =
  let task =
    Task.create ~id ~domain ~parent_id
      (Runtime_events.Timestamp.to_int64 ts)
      kind
  in
  Task_tree.add tasks task

let update_loc i loc =
  Task_tree.update tasks (Task.Id.eio_of_int i) (fun t ->
      { t with Task.loc = loc :: t.loc })

let update_logs i logs =
  Task_tree.update tasks (Task.Id.eio_of_int i) (fun t ->
      { t with Task.logs = logs :: t.logs })

let update_name i name =
  Task_tree.update tasks (Task.Id.eio_of_int i) (fun t ->
      { t with Task.name = name :: t.name })

let switch_to ~id ~domain:_ ts =
  Task_tree.update_active tasks ~id:(Task.Id.eio_of_int id)
    (Runtime_events.Timestamp.to_int64 ts)

let resolved v ts =
  Task_tree.update tasks (Task.Id.eio_of_int v) (fun t ->
      { t with status = Resolved (Runtime_events.Timestamp.to_int64 ts) })

let terminated status ts =
  let reason_str =
    match status with
    | Unix.WEXITED code -> Fmt.str "exited (%d)" code
    | Unix.WSIGNALED s -> Fmt.str "signaled (%d)" s
    | Unix.WSTOPPED s -> Fmt.str "stopped (%d)" s
  in
  Logs.info (fun f -> f "Child process terminated with status %s" reason_str);
  Task_tree.iter_mut tasks (fun t -> { t with status = Resolved ts })

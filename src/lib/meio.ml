module Ctf = Eio.Private.Ctf

let add_callback = Runtime_events.Callbacks.add_user_event
let timestamp s = Runtime_events.Timestamp.to_int64 s |> Int64.to_string

let task_events ~latency_begin ~latency_end q =
  let module Queue = Eio_utils.Lf_queue in
  let evs =
    Runtime_events.Callbacks.create ~runtime_begin:latency_begin
      ~runtime_end:latency_end ()
  in
  let id_event_callback d ts c ((i : Ctf.id), v) =
    match (Runtime_events.User.tag c, v) with
    | Ctf.Created, Ctf.Task -> Queue.push q (`Created ((i :> int), d, ts))
    | _ -> ()
  in
  let id_callback d ts c i =
    match Runtime_events.User.tag c with
    | Ctf.Resolved -> Queue.push q (`Resolved (i, d, ts))
    | Ctf.Switch -> Queue.push q (`Switch ((i :> int), d, ts))
    | _ -> ()
  in
  let id_label_callback _d _ts c (i, s) =
    match Runtime_events.User.tag c with
    | Ctf.Label -> Queue.push q (`Labelled (i, s))
    | _ -> ()
  in
  add_callback Ctf.created_type id_event_callback evs
  |> add_callback Runtime_events.Type.int id_callback
  |> add_callback Ctf.labelled_type id_label_callback

let get_selected () =
  Task_table.find_first
    (fun v -> v.Task.selected)
    (Lwd_table.first State.tasks.table)
  |> fun v -> Option.bind v Lwd_table.get |> Option.get

let screens duration hist =
  [
    (`Help, fun () -> Lwd.return Help.help);
    (`Main, fun () -> Console.root ());
    ( `Task,
      fun () ->
        Nottui_widgets.scroll_area @@ Lwd.return @@ Task.ui @@ get_selected ()
    );
    (`Gc, fun () -> Latency.ui hist duration);
  ]

let ui handle =
  let module Queue = Eio_utils.Lf_queue in
  let q = Queue.create () in
  let cursor = Runtime_events.create_cursor (Some handle) in
  let callbacks = task_events q in
  let screen = Lwd.var `Main in
  let duration = Lwd.var 0L in
  let hist, latency_begin, latency_end = Latency.init () in
  let screens = screens duration hist in
  let ui =
    Lwd.bind ~f:(fun screen -> (List.assoc screen screens) ()) (Lwd.get screen)
  in
  let ui =
    Lwd.map2
      ~f:(fun ui s ->
        Nottui.Ui.event_filter
          (function
            | `Key (`Arrow `Down, _) when s = `Main ->
                Console.set_selected `Prev (Lwd_table.first State.tasks.table);
                `Handled
            | `Key (`Arrow `Up, _) when s = `Main ->
                Console.set_selected `Next (Lwd_table.first State.tasks.table);
                `Handled
            | `Key (`ASCII 'h', _) ->
                Lwd.set screen `Help;
                `Handled
            | `Key (`ASCII 'e', _) ->
                Lwd.set screen `Task;
                `Handled
            | `Key (`ASCII 'm', _) ->
                Lwd.set screen `Main;
                `Handled
            | `Key (`ASCII 'g', _) ->
                Lwd.set screen `Gc;
                `Handled
            | _ -> `Unhandled)
          ui)
      ui (Lwd.get screen)
  in
  Nottui.Ui_loop.run
    ~tick:(fun () ->
      Console.set_prev_now (Timestamp.current ());
      let now = Lwd.peek duration in
      Lwd.set duration
        ( Lwd.peek Console.prev_now |> fun (p, n) ->
          Int64.add now (Int64.sub n p) );
      let _ =
        Runtime_events.read_poll cursor
          (callbacks ~latency_begin ~latency_end)
          None
      in
      while not (Queue.is_empty q) do
        match Queue.pop q with
        | None -> ()
        | Some (`Created v) -> State.add_tasks v
        | Some (`Switch (v, domain, _)) ->
            State.switch_to ~id:(v :> int) ~domain
        | Some (`Resolved (_v, _, _)) ->
            () (* XXX: When to do this State.remove_task v ?  *)
        | Some (`Labelled (i, l)) -> State.update_loc (i :> int) l
      done)
    ~tick_period:0.05 ui

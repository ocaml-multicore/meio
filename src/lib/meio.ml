module Ctf = Eio.Private.Ctf

let add_callback = Runtime_events.Callbacks.add_user_event
let timestamp s = Runtime_events.Timestamp.to_int64 s |> Int64.to_string

let task_events ~latency_begin ~latency_end q =
  let current_id = ref (-1) in
  let module Queue = Eio_utils.Lf_queue in
  let evs =
    Runtime_events.Callbacks.create ~runtime_begin:latency_begin
      ~runtime_end:latency_end
      ~lost_events:(fun domain count ->
        Logs.warn (fun f -> f "[Domain %d] Lost %d events." domain count))
      ()
  in
  let id_event_callback d ts c ((i : Ctf.id), v) =
    match (Runtime_events.User.tag c, v) with
    | Ctf.Created, (Ctf.Task | Ctf.Cancellation_context _) ->
        Queue.push q (`Created ((i :> int), !current_id, d, ts, v))
    | _ -> ()
  in
  let two_ids_callback _d _ts c ((child : Ctf.id), (parent : Ctf.id)) =
    match Runtime_events.User.tag c with
    | Ctf.Parent -> Queue.push q (`Parent ((child :> int), (parent :> int)))
    | _ -> ()
  in
  let unit_callback d ts c () =
    match Runtime_events.User.tag c with
    | Ctf.Suspend ->
        current_id := -1;
        Queue.push q (`Suspend (d, ts))
    | _ -> ()
  in
  let id_callback d ts c i =
    match Runtime_events.User.tag c with
    | Ctf.Resolved -> Queue.push q (`Resolved (i, d, ts))
    | Ctf.Switch ->
        current_id := i;
        Queue.push q (`Switch ((i :> int), d, ts))
    | _ -> ()
  in
  let id_label_callback _d _ts c ((i : Ctf.id), s) =
    match Runtime_events.User.tag c with
    | Ctf.Name -> Queue.push q (`Name ((i :> int), s))
    | Ctf.Log -> Queue.push q (`Log ((i :> int), s))
    | Ctf.Loc -> Queue.push q (`Loc ((i :> int), s))
    | _ -> ()
  in
  add_callback Ctf.created_type id_event_callback evs
  |> add_callback Runtime_events.Type.int id_callback
  |> add_callback Runtime_events.Type.unit unit_callback
  |> add_callback Ctf.labelled_type id_label_callback
  |> add_callback Ctf.two_ids_type two_ids_callback

let get_selected () =
  Task_tree.find_first State.tasks (fun v -> !(v.Task.selected)) |> Option.get

let screens duration hist sort =
  [
    (`Help, fun () -> (Lwd.return Help.help, Lwd.return None));
    (`Main, fun () -> Console.root sort);
    ( `Task,
      fun () ->
        ( Nottui_widgets.scroll_area @@ Lwd.return @@ Task.ui @@ get_selected (),
          Lwd.return None ) );
    (`Gc, fun () -> (Latency.ui hist duration, Lwd.return None));
  ]

let min_thresh = 1e-6
let sleepf f = if f > min_thresh then Unix.sleepf f

module Queue = Eio_utils.Lf_queue

let runtime_event_loop ~child_pid ~q ~stop ~cursor ~callbacks =
  let max_sleep = 0.01 in
  let sleep = ref max_sleep in
  while not (Atomic.get stop) do
    let count = Runtime_events.read_poll cursor callbacks None in
    if count = 0 then (
      (* increase sleep time if there's no events *)
      sleepf !sleep;
      sleep := min max_sleep (!sleep *. 1.2))
    else (
      (* decrease sleep time in case of events *)
      sleep := max min_thresh (!sleep /. 1.2);
      sleepf !sleep);
    if !sleep = max_sleep then
      let child_pid, child_status = Unix.waitpid [ WNOHANG ] child_pid in
      if child_pid <> 0 then (
        Atomic.set stop true;
        Queue.push q (`Terminated child_status))
  done

let ui_loop ~q ~hist =
  let screen = Lwd.var `Main in
  let sort = Lwd.var Sort.Tree in
  let duration = Lwd.var 0L in
  let show_logs = Lwd.var false in
  let screens = screens duration hist (Lwd.get sort) in
  let ui =
    Lwd.bind
      ~f:(fun screen ->
        let a, b = (List.assoc screen screens) () in
        Lwd.pair a b)
      (Lwd.get screen)
  in
  let quit = Lwd.var false in
  let ui =
    Lwd.map2
      ~f:(fun (ui, selected_position) s ->
        Nottui.Ui.event_filter
          (fun ev ->
            match (ev, selected_position) with
            | `Key (`Arrow `Down, _), Some (_, pos, bot, tasks) ->
                Console.set_selected tasks `Next;
                if pos = bot - 1 then `Unhandled else `Handled
            | `Key (`Arrow `Up, _), Some (top, pos, _, tasks) ->
                Console.set_selected tasks `Prev;
                if pos = top + 1 then `Unhandled else `Handled
            | `Key (`ASCII 'h', _), _ ->
                Lwd.set screen `Help;
                `Handled
            | `Key (`ASCII 'e', _), _ ->
                Lwd.set screen `Task;
                `Handled
            | `Key (`ASCII 'm', _), _ ->
                Lwd.set screen `Main;
                `Handled
            | `Key (`ASCII 'g', _), _ ->
                Lwd.set screen `Gc;
                `Handled
            | `Key (`ASCII 'q', _), _ ->
                Lwd.set quit true;
                `Handled
            | `Key (`ASCII 'l', _), _ ->
                Lwd.set show_logs (Lwd.peek show_logs |> not);
                `Handled
            | `Key (`ASCII 's', _), _ ->
                let s = Sort.next (Lwd.peek sort) in
                Lwd.set sort s;
                `Handled
            | `Key (`Enter, _), _ ->
                Console.toggle_fold_selected State.tasks;
                `Handled
            | `Key (`Escape, _), _ ->
                if s = `Main then Lwd.set quit true else Lwd.set screen `Main;
                `Handled
            | _ -> `Unhandled)
          ui)
      ui (Lwd.get screen)
  in
  let logs =
    Lwd_table.map_reduce
      (fun _ line -> Notty.I.string Notty.A.empty line |> Nottui.Ui.atom)
      Nottui.Ui.pack_y Logging.table
  in
  let ui =
    let open Lwd_infix in
    let$* show_logs = Lwd.get show_logs in
    if show_logs then Nottui_widgets.v_pane ui logs else ui
  in

  let ui =
    Lwd.map2 ~f:Nottui.Ui.join_y ui
      (Help.footer (Lwd.get sort) (Lwd.get screen))
  in

  Logs.info (fun f -> f "UI ready !");
  Nottui.Ui_loop.run ~quit_on_escape:false ~quit
    ~tick:(fun () ->
      Logging.poll ();
      Console.set_prev_now (Timestamp.current ());
      let now = Lwd.peek duration in
      Lwd.set duration
        ( Lwd.peek Console.prev_now |> fun (p, n) ->
          Int64.add now (Int64.sub n p) );
      while not (Queue.is_empty q) do
        match Queue.pop q with
        | None -> ()
        | Some (`Created v) -> State.add_tasks v
        | Some (`Switch (v, domain, ts)) ->
            State.switch_to ~id:(v :> int) ~domain ts
        | Some (`Suspend (domain, ts)) -> State.switch_to ~id:(-1) ~domain ts
        | Some (`Parent (child, parent)) -> State.set_parent ~child ~parent
        | Some (`Resolved (v, _, ts)) ->
            State.resolved (v : int) ts
            (* XXX: When to do this State.remove_task v ?  *)
        | Some (`Loc (i, l)) -> State.update_loc (i :> int) l
        | Some (`Name (i, l)) -> State.update_name (i :> int) l
        | Some (`Log (i, l)) -> State.update_logs (i :> int) l
        | Some (`Terminated status) ->
            State.terminated status (Timestamp.current ())
      done)
    ~tick_period:0.05 ui

let ui ~child_pid handle =
  Logs.set_reporter (Logging.reporter ());
  Logs.set_level (Some Info);
  let q = Queue.create () in
  let cursor = Runtime_events.create_cursor (Some handle) in
  let hist, latency_begin, latency_end = Latency.init () in

  let callbacks = task_events q ~latency_begin ~latency_end in
  let stop = Atomic.make false in
  let domain =
    Domain.spawn (fun () ->
        runtime_event_loop ~child_pid ~q ~stop ~cursor ~callbacks)
  in
  ui_loop ~q ~hist;
  Atomic.set stop true;
  Domain.join domain

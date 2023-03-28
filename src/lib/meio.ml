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
  let unit_callback d ts c () =
    match Runtime_events.User.tag c with
    | Ctf.Suspend -> Queue.push q (`Suspend (d, ts))
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
    | Ctf.Name -> Queue.push q (`Name (i, s))
    | Ctf.Log -> Queue.push q (`Log (i, s))
    | Ctf.Loc -> Queue.push q (`Loc (i, s))
    | _ -> ()
  in
  add_callback Ctf.created_type id_event_callback evs
  |> add_callback Runtime_events.Type.int id_callback
  |> add_callback Runtime_events.Type.unit unit_callback
  |> add_callback Ctf.labelled_type id_label_callback

let get_selected () =
  Task_table.find_first
    (fun v -> v.Task.selected)
    (Lwd_table.first State.tasks.table)
  |> fun v -> Option.bind v Lwd_table.get |> Option.get

let screens duration hist =
  [
    (`Help, fun () -> (Lwd.return Help.help, Lwd.return None));
    (`Main, fun () -> Console.root ());
    ( `Task,
      fun () ->
        ( Nottui_widgets.scroll_area @@ Lwd.return @@ Task.ui @@ get_selected (),
          Lwd.return None ) );
    (`Gc, fun () -> (Latency.ui hist duration, Lwd.return None));
  ]

let sleep f = if f >= 1e-6 then Unix.sleepf f

let runtime_event_loop ~stop ~cursor ~callbacks =
  let max_sleep = 0.01 in
  let sleep = ref max_sleep in
  while not (Atomic.get stop) do
    let count = Runtime_events.read_poll cursor callbacks None in
    if count = 0 then (
      (* increase sleep time if there's no events *)
      Unix.sleepf !sleep;
      sleep := min max_sleep (!sleep *. 1.2))
    else (
      (* decrease sleep time in case of events *)
      sleep := !sleep /. 1.2;
      Unix.sleepf !sleep)
  done

let ui handle =
  let module Queue = Eio_utils.Lf_queue in
  let q = Queue.create () in
  let cursor = Runtime_events.create_cursor (Some handle) in
  let screen = Lwd.var `Main in
  let duration = Lwd.var 0L in
  let hist, latency_begin, latency_end = Latency.init () in
  let screens = screens duration hist in
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
            | `Key (`Arrow `Down, _), Some (_, pos, bot) ->
                Console.set_selected `Prev (Lwd_table.first State.tasks.table);
                if pos = bot - 1 then `Unhandled else `Handled
            | `Key (`Arrow `Up, _), Some (top, pos, _) ->
                Console.set_selected `Next (Lwd_table.first State.tasks.table);
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
            | `Key (`Escape, _), _ ->
                if s = `Main then Lwd.set quit true else Lwd.set screen `Main;
                `Handled
            | _ -> `Unhandled)
          ui)
      ui (Lwd.get screen)
  in

  let callbacks = task_events q ~latency_begin ~latency_end in
  let stop = Atomic.make false in
  let domain =
    Domain.spawn (fun () -> runtime_event_loop ~stop ~cursor ~callbacks)
  in
  Nottui.Ui_loop.run ~quit_on_escape:false ~quit
    ~tick:(fun () ->
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
        | Some (`Resolved (v, _, ts)) ->
            State.resolved (v : int) ts
            (* XXX: When to do this State.remove_task v ?  *)
        | Some (`Loc (i, l)) -> State.update_loc (i :> int) l
        | Some (`Name (i, l)) -> State.update_name (i :> int) l
        | Some (`Log (i, l)) -> State.update_logs (i :> int) l
      done)
    ~tick_period:0.05 ui;
  Atomic.set stop true;
  Domain.join domain

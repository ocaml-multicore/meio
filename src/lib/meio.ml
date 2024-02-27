module RE = Eio_runtime_events

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
  let callback d (ts : Runtime_events.Timestamp.t) (e : RE.event) =
    match e with
    (* |  -> Queue.push q () *)
    | `Create (fiber, `Fiber_in parent) ->
        Queue.push q (`Created ((fiber :> int), parent, d, ts, e))
    | `Create (id, `Cc _) ->
        Queue.push q (`Created ((id :> int), !current_id, d, ts, e));
        (* Bit of a hack -- eio could label this for us? *)
        if id = 0 then Queue.push q (`Name (id, "root"))
    | `Suspend_fiber _ ->
        current_id := -1;
        Queue.push q (`Suspend (d, ts))
    | `Exit_fiber i -> Queue.push q (`Resolved (i, d, ts))
    | `Fiber i ->
        current_id := i;
        Queue.push q (`Switch ((i :> int), d, ts))
    | `Name (i, s) -> Queue.push q (`Name ((i :> int), s))
    | `Log s -> Queue.push q (`Log ((!current_id :> int), s))
    | _ -> ()
  in
  RE.add_callbacks callback evs

let get_selected () =
  Task_tree.find_first_lwd State.tasks (fun v -> !(v.Task.selected))
  |> Lwd.map ~f:Option.get

let screens duration hist sort =
  [
    ( `Help,
      fun () ->
        (Lwd.return (Help.help |> Nottui.Ui.resize ~h:0 ~sh:1), Lwd.return None)
    );
    (`Main, fun () -> Console.root sort);
    ( `Task,
      fun () ->
        ( get_selected ()
          |> Lwd.map ~f:(fun w -> Task.ui w |> Nottui.Ui.resize ~h:0 ~sh:1),
          Lwd.return None ) );
    ( `Gc,
      fun () ->
        ( Latency.ui hist duration |> Lwd.map ~f:(Nottui.Ui.resize ~h:0 ~sh:1),
          Lwd.return None ) );
    ( `Logs,
      fun () ->
        ( Lwd_table.map_reduce
            (fun _ line -> Notty.I.string Notty.A.empty line |> Nottui.Ui.atom)
            Nottui.Ui.pack_y Logging.table,
          Lwd.return None ) );
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
  let screens = screens duration hist (Lwd.get sort) in
  let quit = Lwd.var false in
  let main, task_list = List.assoc `Main screens () in
  let task_list = Lwd.observe task_list in
  let release_queue = Lwd.make_release_queue () in
  let ui =
    Lwd.map2
      ~f:(fun ui s ->
        Nottui.Ui.event_filter
          (fun ev ->
            match (ev, Lwd.sample release_queue task_list) with
            | ( (`Key (`Arrow `Down, _) | `Key (`ASCII 'j', _)),
                Some (_, pos, bot, tasks) ) ->
                Console.set_selected tasks `Next;
                if pos = bot - 1 then `Unhandled else `Handled
            | ( (`Key (`Arrow `Up, _) | `Key (`ASCII 'k', _)),
                Some (top, pos, _, tasks) ) ->
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
                Lwd.set screen `Logs;
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
      main (Lwd.get screen)
  in
  let ui =
    let open Lwd_infix in
    let$* screen = Lwd.get screen in
    if screen <> `Main then
      let pane, _ = (List.assoc screen screens) () in
      Nottui_widgets.v_pane ui pane
    else ui
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
        | Some (`Created (id, parent_id, domain, ts, v)) ->
            State.add_tasks ~id ~parent_id ~domain ts v
        | Some (`Switch (v, domain, ts)) ->
            State.switch_to ~id:(v :> int) ~domain ts
        | Some (`Suspend (domain, ts)) -> State.switch_to ~id:(-1) ~domain ts
        | Some (`Parent (child, parent, ts)) ->
            State.set_parent ~child ~parent ts
        | Some (`Resolved (v, _, ts)) ->
            State.resolved (v : int) ts
            (* XXX: When to do this State.remove_task v ?  *)
        | Some (`Loc (i, l)) -> State.update_loc (i :> int) l
        | Some (`Name (i, l)) -> State.update_name (i :> int) l
        | Some (`Log (i, l)) -> State.update_logs (i :> int) l
        | Some (`Terminated status) ->
            State.terminated status (Timestamp.current ())
      done)
    ~tick_period:0.05 ui;
  Lwd.release release_queue task_list

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

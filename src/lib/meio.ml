module Ctf = Eio.Private.Ctf

let add_callback = Runtime_events.Callbacks.add_user_event
let timestamp s = Runtime_events.Timestamp.to_int64 s |> Int64.to_string

(* Returns the current value of a counter that increments once per nanosecond. *)
external current_timestamp : unit -> int64 = "caml_eio_time_counter"

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
  |> add_callback Runtime_events.Type.counter id_callback
  |> add_callback Ctf.labelled_type id_label_callback

module Task_table = struct
  type sort = Domain | Id | Busy | No_sort

  (* Invariant, the lwd_table is always sorted *)
  type t = { sort : sort; table : Task.t Lwd_table.t }

  let create () = { table = Lwd_table.make (); sort = Busy }

  let filter f row =
    let rec loop = function
      | None -> ()
      | Some row ->
          let next = Lwd_table.next row in
          if f row then () else Lwd_table.remove row;
          loop next
    in
    loop row

  let map f row =
    let rec loop = function
      | None -> ()
      | Some row ->
          let next = Lwd_table.next row in
          let t = f (Option.get (Lwd_table.get row)) in
          Lwd_table.set row t;
          loop next
    in
    loop row

  let iter_with_prev f row =
    let rec loop acc = function
      | None -> ()
      | Some row ->
          let next = Lwd_table.next row in
          let current = Option.get (Lwd_table.get row) in
          f ~prev:acc current;
          (* Lwd_table.set row t; *)
          loop (Some current) next
    in
    loop None row

  let find_first f row =
    let rec loop = function
      | None -> None
      | Some row -> (
          match Lwd_table.get row with
          | None -> loop (Lwd_table.next row)
          | Some v -> if f v then Some row else loop (Lwd_table.next row))
    in
    loop row

  let remove_by_id { table; _ } id =
    filter
      (fun t ->
        match Lwd_table.get t with
        | Some t -> Int.equal t.Task.id id
        | None -> true)
      (Lwd_table.first table)

  let update_loc { table; _ } id info =
    map
      (fun t ->
        if Int.equal t.Task.id id then { t with info = info :: t.info } else t)
      (Lwd_table.first table)

  let update_active { table; _ } ~domain ~id =
    map
      (fun t ->
        if Int.equal t.Task.id id && Int.equal t.Task.domain domain then
          { t with active = true; busy = ref 0L :: t.busy }
        else if Int.equal t.Task.domain domain then { t with active = false }
        else t)
      (Lwd_table.first table)

  let find_sort_row t v = function
    | Busy ->
        Int64.compare (Task.get_current_busy t) (Task.get_current_busy v) < 0
    | _ -> true

  let add ({ table; sort; _ } : t) task =
    let set_selected = Option.is_none @@ Lwd_table.first table in
    let first =
      find_first (fun v -> find_sort_row task v sort) (Lwd_table.first table)
    in
    (* Hmmmm *)
    (match first with
    | None -> Lwd_table.append' table task
    | Some row -> ignore (Lwd_table.before ~set:task row));
    if set_selected then task.selected <- true

  let refresh_active_tasks diff ({ table; _ } as t) =
    let rec collect_and_remove acc = function
      | None -> List.rev acc
      | Some row -> (
          match Lwd_table.get row with
          | None -> collect_and_remove acc (Lwd_table.next row)
          | Some (task : Task.t) ->
              let next = Lwd_table.next row in
              let acc =
                if task.active then (
                  let busy = List.hd task.busy in
                  busy := Int64.add !busy diff;
                  Lwd_table.remove row;
                  task :: acc)
                else acc
              in
              collect_and_remove acc next)
    in
    let active = collect_and_remove [] (Lwd_table.first table) in
    List.iter (add t) active
end

module Console = struct
  open Nottui
  module W = Nottui_widgets

  (* Mutable State *)
  let tasks = Task_table.create ()
  let gravity_pad = Gravity.make ~h:`Negative ~v:`Negative

  let prev_now =
    let ts = current_timestamp () in
    Lwd.var (ts, ts)

  let set_prev_now now =
    let really_old, old = Lwd.peek prev_now in
    (* Update active tasks *)
    Task_table.refresh_active_tasks Int64.(sub old really_old) tasks;
    Lwd.set prev_now (old, now)

  let set_selected = function
    | `Next ->
        let set = ref false in
        Task_table.iter_with_prev (fun ~prev c ->
            if !set then ()
            else
              match prev with
              | None -> ()
              | Some p ->
                  if p.Task.selected then (
                    p.selected <- false;
                    c.Task.selected <- true;
                    set := true))
    | `Prev ->
        Task_table.iter_with_prev (fun ~prev c ->
            match prev with
            | None -> ()
            | Some p ->
                if c.Task.selected then (
                  c.selected <- false;
                  p.selected <- true))

  let width = 12
  let padding = 3

  let set_column_widths acc ws =
    assert (List.length ws = List.length acc);
    let f w w' =
      let w = Ui.layout_width w in
      max w w'
    in
    let new_widths = List.map2 f ws acc in
    new_widths

  let green = W.string ~attr:Notty.A.(bg green)
  let seleted_attr = Notty.A.(bg cyan)

  let resize_uis widths (selected, uis) =
    let bg = if selected then Some seleted_attr else None in
    List.map2 (fun w ui -> Ui.resize ?bg ~w ~pad:gravity_pad ui) widths uis

  let render_task now _
      ({ Task.id; domain; start; info; busy; selected; _ } as t) =
    let attr =
      match selected with false -> None | true -> Some seleted_attr
    in
    let domain = W.int ?attr domain in
    let id = W.int ?attr id in
    let total = Int64.sub now start in
    let total_busy = List.fold_left (fun acc v -> Int64.add acc !v) 0L busy in
    let idle = max 0L (Int64.sub total total_busy) in
    let busy = W.string ?attr @@ Fmt.(to_to_string uint64_ns_span total_busy) in
    let idle = W.string ?attr @@ Fmt.(to_to_string uint64_ns_span idle) in
    let loc = W.string ?attr (String.concat "\n" info) in
    let entered = W.int ?attr (List.length t.busy) in
    [ (Option.is_some attr, [ domain; id; busy; idle; entered; loc ]) ]

  let ui_monoid_list : (bool * ui list) list Lwd_utils.monoid = ([], List.append)

  let header =
    [
      green "DOMAIN";
      green "ID";
      green "BUSY";
      green "IDLE";
      green "ENTER";
      green "INFO";
    ]

  let init_widths = List.init (List.length header) (fun _ -> width)

  let root () =
    let task_list =
      Lwd.bind
        ~f:(fun (_, now) ->
          Lwd_table.map_reduce (render_task now) ui_monoid_list tasks.table)
        (Lwd.get prev_now)
    in
    let widths =
      Lwd.map
        ~f:(fun uis ->
          let widths =
            List.fold_left
              (fun acc (_, w) -> set_column_widths acc w)
              init_widths uis
          in
          widths)
        task_list
    in
    let table =
      Lwd.map2
        ~f:(fun widths ts ->
          let widths = List.map (( + ) padding) widths in
          List.fold_left
            (fun acc ui ->
              Ui.join_y
                (List.fold_left Ui.join_x Ui.empty (resize_uis widths ui))
                acc)
            Ui.empty ts)
        widths task_list
    in
    let table_header =
      Lwd.map widths ~f:(fun w ->
          List.map2
            (fun w ui ->
              Ui.resize
                ~bg:Notty.A.(bg green)
                ~w:(w + padding) ~pad:gravity_pad ui)
            w header)
      |> Lwd.map ~f:(List.fold_left Ui.join_x Ui.empty)
    in
    let footer = Lwd_utils.pack Ui.pack_x (List.map Lwd.pure Help.footer) in
    Lwd_utils.pack Ui.pack_y [ table_header; table |> W.scroll_area; footer ]

  let add_tasks (id, domain, ts) =
    let task = Task.create ~id ~domain (Runtime_events.Timestamp.to_int64 ts) in
    Task_table.add tasks task

  let remove_task i = Task_table.remove_by_id tasks i
  let update_loc i id = Task_table.update_loc tasks i id
  let switch_to ~id ~domain = Task_table.update_active tasks ~id ~domain
end

let get_selected () =
  Task_table.find_first
    (fun v -> v.Task.selected)
    (Lwd_table.first Console.tasks.table)
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
                Console.set_selected `Prev (Lwd_table.first Console.tasks.table);
                `Handled
            | `Key (`Arrow `Up, _) when s = `Main ->
                Console.set_selected `Next (Lwd_table.first Console.tasks.table);
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
      Console.set_prev_now (current_timestamp ());
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
        | Some (`Created v) -> Console.add_tasks v
        | Some (`Switch (v, domain, _)) ->
            Console.switch_to ~id:(v :> int) ~domain
        | Some (`Resolved (_v, _, _)) ->
            () (* XXX: When to do this Console.remove_task v ?  *)
        | Some (`Labelled (i, l)) -> Console.update_loc (i :> int) l
      done)
    ~tick_period:0.15 ui

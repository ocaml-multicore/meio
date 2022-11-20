module Ctf = Eio.Private.Ctf

let add_callback = Runtime_events.Callbacks.add
let timestamp s = Runtime_events.Timestamp.to_int64 s |> Int64.to_string

(* Returns the current value of a counter that increments once per nanosecond. *)
external current_timestamp : unit -> int64 = "caml_eio_time_counter"

let task_events q =
  let module Queue = Eio_utils.Lf_queue in
  let evs = Runtime_events.Callbacks.create () in
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

module Task = struct
  module W = Nottui_widgets

  type t = {
    id : int;
    domain : int;
    start : int64;
    mutable busy : int64;
    mutable entered_count : int;
    mutable info : string list; (* include location *)
    active : bool;
  }

  let create ~id ~domain start =
    {
      id;
      domain;
      start;
      busy = 0L;
      entered_count = 0;
      info = [];
      active = false;
    }
end

module Task_table = struct
  type t = Task.t Lwd_table.t

  let create () = Lwd_table.make ()
  let add (tbl : t) task = Lwd_table.append' tbl task

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

  let remove_by_id tbl id =
    filter
      (fun t ->
        match Lwd_table.get t with
        | Some t -> Int.equal t.Task.id id
        | None -> true)
      (Lwd_table.first tbl)

  let update_loc tbl id info =
    map
      (fun t ->
        if Int.equal t.Task.id id then { t with info = info :: t.info } else t)
      (Lwd_table.first tbl)

  let update_active tbl ~domain ~id =
    map
      (fun t ->
        if Int.equal t.Task.id id && Int.equal t.Task.domain domain then
          { t with active = true; entered_count = t.entered_count + 1 }
        else if Int.equal t.Task.domain domain then { t with active = false }
        else t)
      (Lwd_table.first tbl)
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
    let _, old = Lwd.peek prev_now in
    Lwd.set prev_now (old, now)

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

  let resize_uis widths uis =
    List.map2 (fun w ui -> Ui.resize ~w ~pad:gravity_pad ui) widths uis

  let green = W.string ~attr:Notty.A.(bg green)

  let render_task (prev, now) _
      ({ Task.id; domain; start; info; active; busy; _ } as t) =
    let busy =
      if active then (
        t.busy <- Int64.add busy Int64.(sub now prev);
        t.busy)
      else busy
    in
    let domain = W.int domain in
    let id = W.int id in
    let total = Int64.sub now start in
    let idle = max 0L (Int64.sub total busy) in
    let busy = W.string @@ Fmt.(to_to_string uint64_ns_span busy) in
    let idle = W.string @@ Fmt.(to_to_string uint64_ns_span idle) in
    let loc = W.string (String.concat "\n" info) in
    let entered = W.int t.entered_count in
    [ [ domain; id; busy; idle; entered; loc ] ]

  let ui_monoid_list : ui list list Lwd_utils.monoid = ([], List.append)

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
    let task_list : ui list list Lwd.t =
      Lwd.bind
        ~f:(fun now ->
          Lwd_table.map_reduce (render_task now) ui_monoid_list tasks)
        (Lwd.get prev_now)
    in
    let widths =
      Lwd.map
        ~f:(fun uis ->
          let widths =
            List.fold_left
              (fun acc w -> set_column_widths acc w)
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
    Lwd_utils.pack Ui.pack_y [ table_header; table |> W.scroll_area ]

  let add_tasks (id, domain, ts) =
    let task = Task.create ~id ~domain (Runtime_events.Timestamp.to_int64 ts) in
    Task_table.add tasks task

  let remove_task i = Task_table.remove_by_id tasks i
  let update_loc i id = Task_table.update_loc tasks i id
  let switch_to ~id ~domain = Task_table.update_active tasks ~id ~domain
end

let ui handle =
  let module Queue = Eio_utils.Lf_queue in
  let q = Queue.create () in
  let cursor = Runtime_events.create_cursor (Some handle) in
  let callbacks = task_events q in
  Nottui.Ui_loop.run
    ~tick:(fun () ->
      Console.set_prev_now (current_timestamp ());
      let _ = Runtime_events.read_poll cursor callbacks None in
      while not (Queue.is_empty q) do
        match Queue.pop q with
        | None -> ()
        | Some (`Created v) -> Console.add_tasks v
        | Some (`Switch (v, domain, _)) ->
            Console.switch_to ~id:(v :> int) ~domain
        | Some (`Resolved (_v, _, _)) -> () (* Console.remove_task v *)
        | Some (`Labelled (i, l)) -> Console.update_loc (i :> int) l
      done)
    ~tick_period:0.1 (Console.root ())

open Nottui
module W = Nottui_widgets

let gravity_pad = Gravity.make ~h:`Negative ~v:`Negative

let prev_now =
  let ts = Timestamp.current () in
  Lwd.var (ts, ts)

let set_prev_now now =
  let _, old = Lwd.peek prev_now in
  Lwd.set prev_now (old, now)

let toggle_fold_selected t =
  Task_tree.iter t (fun c ->
      if c.Task.selected then
        c.display <-
          (match c.display with
          | Toggle_requested -> Toggle_requested
          | Yes -> No
          | No -> Yes
          | Auto -> Toggle_requested))

let set_selected tasks action =
  match action with
  | `Next ->
      let rec loop = function
        | [] -> ()
        | [ _ ] -> ()
        | prev :: c :: rest when prev.Task.selected ->
            prev.selected <- false;
            c.Task.selected <- true
        | _ :: rest -> loop rest
      in
      loop tasks
  | `Prev ->
      let rec loop = function
        | [] -> ()
        | [ _ ] -> ()
        | prev :: c :: rest when c.Task.selected ->
            c.selected <- false;
            prev.Task.selected <- true
        | _ :: rest -> loop rest
      in
      loop tasks

let width = 6
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
let selected_attr = Notty.A.(bg cyan)
let resolved_attr = Notty.A.(fg (gray 10))

let resize_uis widths (bg, _, uis, _) =
  List.map2 (fun w ui -> Ui.resize ~bg ~w ~pad:gravity_pad ui) widths uis

let attr' selected active =
  let open Notty.A in
  let fg_attr = match active with true -> empty | false -> fg (gray 10) in
  if selected then selected_attr ++ fg_attr else fg_attr

let render_tree_line ~filtered depth is_active attr =
  let depth = List.rev depth in
  let l = List.length depth in
  List.mapi
    (fun i { Task_tree.last; active } ->
      match (i = l - 1, last, filtered) with
      | true, false, false -> (" ├─ ", is_active)
      | true, true, false -> (" └─ ", is_active)
      | true, false, true -> (" ╞═ ", is_active)
      | true, true, true -> (" ╘═ ", is_active)
      | false, false, _ -> (" │ ", active)
      | false, true, _ -> ("   ", false))
    depth
  |> List.map (fun (s, is_active) -> W.string ~attr:(attr is_active) s)
  |> Ui.hcat

let cancellation_context_purpose_to_string = function
  | Eio.Private.Ctf.Choose -> "choose"
  | Pick -> "pick"
  | Join -> "join"
  | Switch -> "switch"
  | Protect -> "protect"
  | Sub -> "sub"
  | Root -> "root"

let render_task sort now ~depth ~filtered
    ({ Task.id; domain; start; loc; name; busy; selected; status; kind; _ } as
    t) =
  let is_active = Task.is_active t in
  let attr = attr' selected is_active in
  let attr =
    let open Notty.A in
    match status with Active _ -> attr ++ st bold | _ -> attr
  in
  let now = match status with Resolved v -> v | _ -> now in
  let domain = W.int ~attr domain in
  let id = W.int ~attr id in
  let total = Int64.sub now start in
  let color_busy =
    match status with
    | Active v -> Some Notty.A.white
    | Paused since ->
        let scale =
          255 - (Int64.div (Int64.sub now since) 2_000_000L |> Int64.to_int)
        in
        let scale =
          if scale < 0 then 0 else if scale > 255 then 255 else scale
        in
        Some (Notty.A.rgb_888 ~r:scale ~g:scale ~b:scale)
    | _ -> None
  in
  let active_busy = match status with Active v -> Int64.sub now v | _ -> 0L in
  let total_busy = Int64.add active_busy (Task.Busy.total busy) in
  let idle = max 0L (Int64.sub total total_busy) in
  let busy = W.string ~attr @@ Fmt.(str " %a" uint64_ns_span total_busy) in
  let busy =
    let busy_box =
      match color_busy with
      | Some color -> W.string ~attr:Notty.A.(attr ++ fg color) "●"
      | None -> W.string " "
    in
    Ui.join_x busy_box busy
  in
  let idle = W.string ~attr @@ Fmt.(to_to_string uint64_ns_span idle) in
  let loc = W.string ~attr (String.concat "\n" loc) in
  let name = W.string ~attr (String.concat "\n" name) in
  let entered = W.int ~attr (Task.Busy.count t.busy) in
  let name =
    if sort = Sort.Tree then
      Ui.hcat
        [ render_tree_line ~filtered depth is_active (attr' selected); name ]
    else name
  in
  let kind =
    W.string ~attr
      (match kind with
      | Cancellation_context { protected; purpose } ->
          Fmt.str "cc {%b, %s}" protected
            (cancellation_context_purpose_to_string purpose)
      | Task -> "task"
      | _ -> "??")
  in
  (attr, selected, [ domain; id; kind; name; busy; idle; entered; loc ], t)

let ui_monoid_list :
    (Notty.attr * bool * ui list * Task.t) list Lwd_utils.monoid =
  ([], List.append)

let header =
  [
    green "DOMAIN";
    green "ID";
    green "KIND";
    green "NAME";
    green "BUSY";
    green "IDLE";
    green "ENTER";
    green "INFO";
  ]

let init_widths = List.init (List.length header) (fun _ -> width)

let root sort =
  let task_list =
    let open Lwd_infix in
    let$ _, now = Lwd.get prev_now and$ sort = sort in
    Task_tree.flatten State.tasks (render_task sort now)
    |> List.of_seq
    |>
    match sort with
    | Sort.Tree -> Fun.id
    | some_sort ->
        List.sort (fun (_, _, _, t1) (_, _, _, t2) -> Sort.compare sort t1 t2)
  in

  let widths =
    Lwd.map
      ~f:(fun uis ->
        let widths =
          List.fold_left
            (fun acc (_, _, w, _) -> set_column_widths acc w)
            init_widths uis
        in
        widths)
      task_list
  in
  let sensor_y var ~x:_ ~y ~w:_ ~h:_ () =
    if Lwd.peek var <> y then Lwd.set var y
  in
  let h_top = Lwd.var 0 in
  let h_selected = Lwd.var 0 in
  let h_bottom = Lwd.var 0 in
  let table =
    let open Lwd_infix in
    let$ widths = widths and$ ts = task_list in
    let widths = List.map (( + ) padding) widths in
    List.fold_left
      (fun acc ui ->
        let line = List.fold_left Ui.join_x Ui.empty (resize_uis widths ui) in
        let line =
          match ui with
          | _, true, _, _ -> Ui.transient_sensor (sensor_y h_selected) line
          | _, _, _, _ -> line
        in
        Ui.join_y acc line)
      Ui.empty ts
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
  let selected_position =
    let open Lwd_infix in
    let$ top = Lwd.get h_top
    and$ selected = Lwd.get h_selected
    and$ bottom = Lwd.get h_bottom
    and$ tasks = task_list in
    Some (top, selected, bottom, tasks |> List.map (fun (_, _, _, t) -> t))
  in
  let footer = Lwd.map ~f:Ui.hcat (Help.footer sort) in
  ( [
      W.vbox
        [
          Lwd.map ~f:(Ui.permanent_sensor (sensor_y h_top)) table_header;
          table
          |> Nottui_extended.scroll_area ~direction:`Vertical
          |> Lwd.map ~f:(Ui.resize ~h:0 ~sh:1);
        ]
      |> Nottui_extended.scroll_area ~direction:`Horizontal
      |> W.scrollbox;
      Lwd.map ~f:(Ui.permanent_sensor (sensor_y h_bottom)) footer;
    ]
    |> W.vbox,
    selected_position )

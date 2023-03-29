open Nottui
module W = Nottui_widgets

let gravity_pad = Gravity.make ~h:`Negative ~v:`Negative

let prev_now =
  let ts = Timestamp.current () in
  Lwd.var (ts, ts)

let set_prev_now now =
  let _, old = Lwd.peek prev_now in
  Lwd.set prev_now (old, now)

let set_selected t = function
  | `Next ->
      let set = ref false in
      Task_table.iter_with_prev t (fun ~prev c ->
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
      Task_table.iter_with_prev t (fun ~prev c ->
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
let selected_attr = Notty.A.(bg cyan)
let resolved_attr = Notty.A.(fg (gray 10))

let resize_uis widths (bg, _, uis) =
  List.map2 (fun w ui -> Ui.resize ~bg ~w ~pad:gravity_pad ui) widths uis

let render_tree_line depth attr =
  let line =
    let depth = List.tl (List.rev depth) in
    let l = List.length depth in
    List.mapi
      (fun i v ->
        match (i = l - 1, v) with
        | true, false -> " ├─"
        | true, true -> " └─"
        | false, false -> " │ "
        | false, true -> "   ")
      depth
    |> String.concat ""
  in
  W.string ~attr (line ^ " ")

let render_task sort now _
    ({ Task.id; domain; start; loc; name; busy; selected; status; depth; _ } as
    t) =
  let attr =
    let open Notty.A in
    let fg_attr =
      match status with
      | Resolved _ -> fg (gray 10)
      | Active _ -> st bold
      | _ -> empty
    in
    if selected then selected_attr ++ fg_attr else fg_attr
  in
  let now = match status with Resolved v -> v | _ -> now in
  let domain = W.int ~attr domain in
  let id = W.int ~attr id in
  let total = Int64.sub now start in
  let active_busy = match status with Active v -> Int64.sub now v | _ -> 0L in
  let total_busy = Int64.add active_busy (Task.Busy.total busy) in
  let idle = max 0L (Int64.sub total total_busy) in
  let busy = W.string ~attr @@ Fmt.(to_to_string uint64_ns_span total_busy) in
  let idle = W.string ~attr @@ Fmt.(to_to_string uint64_ns_span idle) in
  let loc = W.string ~attr (String.concat "\n" loc) in
  let name = W.string ~attr (String.concat "\n" name) in
  let entered = W.int ~attr (Task.Busy.count t.busy) in
  let name =
    if sort = Sort.Tree then Ui.hcat [ render_tree_line depth attr; name ]
    else name
  in
  [ (attr, selected, [ domain; id; name; busy; idle; entered; loc ]) ]

let ui_monoid_list : (Notty.attr * bool * ui list) list Lwd_utils.monoid =
  ([], List.append)

let header =
  [
    green "DOMAIN";
    green "ID";
    green "NAME";
    green "BUSY";
    green "IDLE";
    green "ENTER";
    green "INFO";
  ]

let init_widths = List.init (List.length header) (fun _ -> width)

let root sort =
  let task_list =
    Lwd.bind
      ~f:(fun ((_, now), sort) ->
        Lwd_table.map_reduce (render_task sort now) ui_monoid_list
          State.tasks.table)
      (Lwd.pair (Lwd.get prev_now) sort)
  in
  let widths =
    Lwd.map
      ~f:(fun uis ->
        let widths =
          List.fold_left
            (fun acc (_, _, w) -> set_column_widths acc w)
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
    Lwd.map2
      ~f:(fun widths ts ->
        let widths = List.map (( + ) padding) widths in
        List.fold_left
          (fun acc ui ->
            let line =
              List.fold_left Ui.join_x Ui.empty (resize_uis widths ui)
            in
            let line =
              match ui with
              | _, true, _ -> Ui.transient_sensor (sensor_y h_selected) line
              | _, _, _ -> line
            in
            Ui.join_y line acc)
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
  let selected_position =
    let open Lwd_infix in
    let$ top = Lwd.get h_top
    and$ selected = Lwd.get h_selected
    and$ bottom = Lwd.get h_bottom in
    Some (top, selected, bottom)
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

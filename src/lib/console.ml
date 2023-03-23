open Nottui
module W = Nottui_widgets

let gravity_pad = Gravity.make ~h:`Negative ~v:`Negative

let prev_now =
  let ts = Timestamp.current () in
  Lwd.var (ts, ts)

let set_prev_now now =
  let really_old, old = Lwd.peek prev_now in
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
let selected_attr = Notty.A.(bg cyan)
let resolved_attr = Notty.A.(fg (gray 10))

let resize_uis widths (bg, uis) =
  List.map2 (fun w ui -> Ui.resize ~bg ~w ~pad:gravity_pad ui) widths uis

let render_task now _
    ({ Task.id; domain; start; info; busy; selected; status; _ } as t) =
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
  let total_busy = List.fold_left Int64.add active_busy busy in
  let idle = max 0L (Int64.sub total total_busy) in
  let busy = W.string ~attr @@ Fmt.(to_to_string uint64_ns_span total_busy) in
  let idle = W.string ~attr @@ Fmt.(to_to_string uint64_ns_span idle) in
  let loc = W.string ~attr (String.concat "\n" info) in
  let entered = W.int ~attr (List.length t.busy) in
  [ (attr, [ domain; id; busy; idle; entered; loc ]) ]

let ui_monoid_list : (Notty.attr * ui list) list Lwd_utils.monoid =
  ([], List.append)

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
        Lwd_table.map_reduce (render_task now) ui_monoid_list State.tasks.table)
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

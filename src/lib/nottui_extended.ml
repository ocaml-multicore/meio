let mini, maxi, clampi = Lwd_utils.(mini, maxi, clampi)
let scroll_step = 1

module Ui = Nottui.Ui

let scroll_area ?(direction = `Both) ?(offset = (0, 0)) t =
  let offset = Lwd.var offset in
  let content_w_h = Lwd.var (0, 0) in
  let viewport_w_h = Lwd.var (0, 0) in
  let scroll d_x d_y =
    let s_x, s_y = Lwd.peek offset in
    let cw, ch = Lwd.peek content_w_h in
    let vw, vh = Lwd.peek viewport_w_h in
    (* Printf.printf "\r c%d v%d%!" ch vh;
       Unix.sleepf 0.2; *)
    let s_x = clampi ~min:0 ~max:(maxi 0 (cw - vw)) (s_x + d_x) in
    let s_y = clampi ~min:0 ~max:(maxi 0 (ch - vh)) (s_y + d_y) in
    Lwd.set offset (s_x, s_y);
    `Handled
  in
  let focus_handler (a, b) =
    match (a, b, direction) with
    | `Arrow `Left, [], (`Horizontal | `Both) -> scroll (-scroll_step) 0
    | `Arrow `Right, [], (`Horizontal | `Both) -> scroll (+scroll_step) 0
    | `Arrow `Up, [], (`Vertical | `Both) -> scroll 0 (-scroll_step)
    | `Arrow `Down, [], (`Vertical | `Both) -> scroll 0 (+scroll_step)
    | `Page `Up, [], (`Vertical | `Both) -> scroll 0 (-scroll_step * 8)
    | `Page `Down, [], (`Vertical | `Both) -> scroll 0 (+scroll_step * 8)
    | _ -> `Unhandled
  in
  let scroll_handler ~x:_ ~y:_ z =
    match (z, direction) with
    | `Scroll `Up, (`Vertical | `Both) -> scroll 0 (-scroll_step)
    | `Scroll `Down, (`Vertical | `Both) -> scroll 0 (+scroll_step)
    | _ -> `Unhandled
  in
  let t =
    Lwd.map
      ~f:(fun v ->
        Lwd.set content_w_h (Ui.layout_width v, Ui.layout_height v);
        v)
      t
  in
  let d_x i =
    match direction with `Horizontal -> i | `Vertical -> 0 | `Both -> i
  in
  let d_y i =
    match direction with `Horizontal -> 0 | `Vertical -> i | `Both -> i
  in
  Lwd.map2 t (Lwd.get offset) ~f:(fun t (s_x, s_y) ->
      t
      |> Ui.shift_area (d_x s_x) (d_y s_y)
      |> Ui.mouse_area scroll_handler
      |> Ui.keyboard_area focus_handler
      |> Ui.resize ~w:0 ~h:0 ~sw:1 ~sh:1
      |> Ui.size_sensor (fun ~w ~h -> Lwd.set viewport_w_h (w, h)))

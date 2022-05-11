open Js_of_ocaml

let view v name =
  try
    match Dom_html.tagged (Dom_html.getElementById name) with
    | Dom_html.Canvas c -> Html_viewer.attach ~grab_focus:true c v
    | _ -> raise Not_found
  with Not_found ->
    failwith (Printf.sprintf "Canvas element '%s' not found in DOM" name)
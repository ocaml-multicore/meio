open Brr

let dygraphs = Jv.get Jv.global "Dygraph"

type opts = Jv.t
type data = Jv.t

let data_of_string s = Jv.of_string s
let data_of_array arr = Jv.of_array (Jv.of_array Jv.of_float) arr
let data_of_jv v = Jv.Id.of_jv v

let legend_opt_to_jv = function
  | `Always -> Jv.of_string "always"
  | `Follow -> Jv.of_string "follow"

let opts ?draw_points ?show_roller ?value_range ?file ?legend ?labels ?title
    ?ylabel ?connect_separated_points () =
  let o = Jv.obj [||] in
  Jv.Bool.set_if_some o "drawPoints" draw_points;
  Jv.Bool.set_if_some o "showRoller" show_roller;
  Jv.Bool.set_if_some o "connectSeparatedPoints" connect_separated_points;
  Jv.set_if_some o "file" file;
  Option.iter
    (fun (lo, hi) -> Jv.set o "valueRange" (Jv.of_list Jv.of_float [ lo; hi ]))
    value_range;
  Jv.set_if_some o "labels" (Option.map (Jv.of_list Jv.of_string) labels);
  Jv.Jstr.set_if_some o "title" (Option.map Jstr.v title);
  Jv.Jstr.set_if_some o "ylabel" (Option.map Jstr.v ylabel);
  Jv.set_if_some o "legend" (Option.map legend_opt_to_jv legend);
  o

let opts_to_jv = Jv.Id.to_jv
let opts_of_jv = Jv.Id.of_jv

type t = Jv.t

let create ?(opts = Jv.undefined) el data =
  let el = match el with `El el -> El.to_jv el | `Id id -> Jv.of_string id in
  Jv.new' dygraphs [| el; data; opts |]

let update_opts t opts =
  let _jv : Jv.t = Jv.call t "updateOptions" [| opts |] in
  ()

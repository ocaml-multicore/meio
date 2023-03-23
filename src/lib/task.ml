open Nottui
module W = Nottui_widgets
module H = Hdr_histogram

type status = Paused | Active of int64 | Resolved of int64

type t = {
  id : int;
  domain : int;
  start : int64;
  busy : int64 list;
  mutable info : string list; (* include location *)
  mutable selected : bool;
  status : status;
}

let get_current_busy t = match t.busy with [] -> 0L | x :: _ -> x
let equal a b = a.id = b.id && a.domain = b.domain

let create ~id ~domain start =
  { id; domain; start; busy = []; info = []; selected = false; status = Paused }

let percentiles =
  [ 25.0; 50.0; 60.0; 70.0; 80.0; 90.0; 95.0; 99.0; 99.9; 99.99 ]

let max_list =
  List.fold_left (fun max v -> if Int64.compare v max > 0 then v else max) 0L

let make_histogram task =
  let h =
    H.init ~lowest_discernible_value:10 ~highest_trackable_value:10_000_000_000
      ~significant_figures:3
  in
  List.iter (fun v -> assert (H.record_value h (Int64.to_int v))) task.busy;
  h

let ns_span i = Fmt.(to_to_string uint64_ns_span (Int64.of_int i))

let ui task =
  let h = make_histogram task in
  let percentiles =
    let percentiles =
      List.map (fun p -> (p, H.value_at_percentile h p)) percentiles
    in
    List.map (fun (p, i) -> W.fmt "%.2f%% %s" p (ns_span i)) percentiles
  in
  let stats =
    Ui.hcat
      [
        W.fmt
          ~attr:Notty.A.(st italic)
          "max: %s, min: %s, std: %s, mean: %s"
          (ns_span @@ H.max h)
          (ns_span @@ H.min h)
          (ns_span @@ int_of_float @@ H.stddev h)
          (ns_span @@ int_of_float @@ H.mean h);
      ]
  in
  let title =
    W.fmt
      ~attr:Notty.A.(st bold ++ fg green)
      "Task %i in domain %i busy stats" task.id task.domain
  in
  Ui.vcat (title :: stats :: percentiles)

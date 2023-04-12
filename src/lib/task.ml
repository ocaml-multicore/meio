open Nottui
module W = Nottui_widgets
module H = Hdr_histogram

type status = Paused of int64 | Active of int64 | Resolved of int64

module Busy : sig
  type t

  val make : unit -> t
  val total : t -> int64
  val count : t -> int
  val hist : t -> H.t
  val add : t -> int64 -> unit
  val merge : t -> t -> t
end = struct
  type t =
    | Leaf of { mutable total : int64; mutable count : int; hist : H.t }
    | Merge of (t * t)

  let make () =
    Leaf
      {
        total = 0L;
        count = 0;
        hist =
          H.init ~lowest_discernible_value:10
            ~highest_trackable_value:10_000_000_000 ~significant_figures:3;
      }

  let rec total = function
    | Merge (a, b) -> Int64.add (total a) (total b)
    | Leaf t -> t.total

  let rec count = function
    | Merge (a, b) -> count a + count b
    | Leaf t -> t.count

  let hist = function
    | Merge _ -> invalid_arg "Cannot hist merge view"
    | Leaf t -> t.hist

  let add t value =
    match t with
    | Merge _ -> invalid_arg "Cannot add to merge view"
    | Leaf t ->
        assert (H.record_value t.hist (Int64.to_int value));
        t.total <- Int64.add t.total value;
        t.count <- t.count + 1

  let merge a b = Merge (a, b)
end

type display = Auto | Yes | No | Toggle_requested

type t = {
  id : int;
  parent_id : int;
  domain : int;
  start : int64;
  busy : Busy.t;
  name : string list;
  loc : string list;
  logs : string list;
  status : status;
  kind : Eio.Private.Ctf.event;
  mutable selected : bool;
  mutable display : display;
}

let is_active t = match t.status with Resolved _ -> false | _ -> true
let get_current_busy t = Busy.total t.busy
let equal a b = a.id = b.id && a.domain = b.domain

let create ~id ~domain ~parent_id start kind =
  {
    id;
    domain;
    parent_id;
    start;
    busy = Busy.make ();
    logs = [];
    name = [];
    loc = [];
    selected = false;
    display = Auto;
    status = Paused 0L;
    kind;
  }

let percentiles =
  [ 25.0; 50.0; 60.0; 70.0; 80.0; 90.0; 95.0; 99.0; 99.9; 99.99 ]

let max_list =
  List.fold_left (fun max v -> if Int64.compare v max > 0 then v else max) 0L

let make_histogram task = Busy.hist task.busy
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

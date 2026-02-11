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

module Id = struct
  type t = { eio : int; counter : int; global_counter : int ref }

  let make eio = { eio; counter = 0; global_counter = ref 0 }

  let fork t =
    incr t.global_counter;
    { t with counter = !(t.global_counter) }

  let pp fmt t =
    if t.counter == 0 then Fmt.int fmt t.eio
    else Fmt.pf fmt "%d.%d" t.eio t.counter

  let compare a b =
    match Int.compare a.eio b.eio with
    | 0 -> Int.compare a.counter b.counter
    | v -> v

  type eio = int

  let pp_eio = Fmt.int
  let eio t = t.eio

  let eio_of_int t =
    assert (t >= -1);
    t
end

type t = {
  id : Id.t;
  parent_id : int;
  domain : int;
  start : int64;
  busy : Busy.t;
  name : string list;
  loc : string list;
  logs : string list;
  status : status;
  kind : Eio_runtime_events.event;
  selected : bool ref;
  display : display ref;
}

let is_active t = match t.status with Resolved _ -> false | _ -> true

let create ~id ~domain ~parent_id start kind =
  {
    id = Id.make id;
    domain;
    parent_id;
    start;
    busy = Busy.make ();
    logs = [];
    name = [];
    loc = [];
    selected = ref false;
    display = ref Auto;
    status = Paused 0L;
    kind;
  }

let percentiles =
  [ 25.0; 50.0; 60.0; 70.0; 80.0; 90.0; 95.0; 99.0; 99.9; 99.99 ]

let make_histogram task = Busy.hist task.busy
let ns_span i = Fmt.(to_to_string uint64_ns_span (Int64.of_int i))

let ui task =
  match task.kind with
  | `Create (_, `Cc _) ->
      W.fmt
        ~attr:Notty.A.(st bold ++ fg green)
        "Cancellation context %a in domain %i" Id.pp task.id task.domain
  | _ ->
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
          "Task %a in domain %i busy stats" Id.pp task.id task.domain
      in
      let logs =
        W.string ~attr:Notty.A.(st bold ++ fg red) "LOGS"
        :: List.map W.string task.logs
      in
      Ui.hcat [ Ui.vcat (title :: stats :: percentiles); Ui.vcat logs ]

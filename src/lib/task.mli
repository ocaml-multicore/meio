module W = Nottui_widgets

type status = Paused of int64 | Active of int64 | Resolved of int64

module Busy : sig
  type t

  val make : unit -> t
  val total : t -> int64
  val count : t -> int
  val hist : t -> Hdr_histogram.t
  val add : t -> int64 -> unit
  val merge : t -> t -> t
end

type display = Auto | Yes | No | Toggle_requested

module Id : sig
  type t
  type extern

  val pp : t Fmt.t
  val compare : t -> t -> int
  val fork : t -> t
  val to_extern : t -> extern
  val pp_extern : extern Fmt.t
  val extern_of_int : int -> extern
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
  kind : Meio_runtime_events.event;
  selected : bool ref;
  display : display ref;
}

val is_active : t -> bool

val create :
  id:int ->
  domain:int ->
  parent_id:int ->
  int64 ->
  Meio_runtime_events.event ->
  t

val ui : t -> Nottui.ui

type t

val make : unit -> t
val add : t -> Task.t -> unit
val update : t -> Task.Id.extern -> (Task.t -> Task.t) -> unit
val update_active : t -> id:Task.Id.extern -> int64 -> unit

val set_parent :
  t -> child:Task.Id.extern -> parent:Task.Id.extern -> int64 -> unit

val iter : t -> (Task.t -> unit) -> unit
val iter_mut : t -> (Task.t -> Task.t) -> unit
val iter_with_prev : t -> (prev:Task.t option -> Task.t -> unit) -> unit

type flatten_info = { last : bool; active : bool; cancellation_context : bool }

val flatten :
  t -> (depth:flatten_info list -> filtered:bool -> Task.t -> 'a) -> 'a Seq.t

val find_first : t -> (Task.t -> bool) -> Task.t option
val find_first_lwd : t -> (Task.t -> bool) -> Task.t option Lwd.t
val invalidate : t -> unit

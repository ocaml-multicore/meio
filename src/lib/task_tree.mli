type t

val make : unit -> t
val add : t -> Task.t -> unit
val update : t -> int -> (Task.t -> Task.t) -> unit
val update_active : t -> id:int -> int64 -> unit
val set_parent : t -> child:int -> parent:int -> unit
val iter : t -> (Task.t -> unit) -> unit
val iter_mut : t -> (Task.t -> Task.t) -> unit
val iter_with_prev : t -> (prev:Task.t option -> Task.t -> unit) -> unit

type flatten_info = { last : bool; active : bool }

val flatten :
  t -> (depth:flatten_info list -> filtered:bool -> Task.t -> 'a) -> 'a Seq.t

val find_first : t -> (Task.t -> bool) -> Task.t option

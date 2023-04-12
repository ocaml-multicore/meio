type t = Domain | Id | Busy | Tree

val next : t -> t
val to_string : t -> string
val compare : t -> Task.t -> Task.t -> int

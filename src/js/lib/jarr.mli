include module type of Jv.Jarray
(** Extending the built-in support for working with
    JavaScript arrays. *)

val push : t -> Jv.t -> unit
val copy : t -> t
val insert : int -> t -> Jv.t -> unit
val null_arr : int -> t

module Syntax : sig
  val ( .:[] ) : t -> int -> Jv.t
  val ( .:[]<- ) : t -> int -> Jv.t -> unit
end

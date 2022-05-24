include module type of Jv.Jarray
(** Extending the built-in support for working with
    JavaScript arrays. *)

val push : t -> Jv.t -> unit
(** [push t v] pushes a new JavaScript value to the array [t]. *)

val copy : t -> t
(** [copy t] creates a separate, new copy of the array [t]. *)

val insert : int -> t -> Jv.t -> unit
(** [insert i t v] adds an element to the array [t] at position [i]. *)

val null_arr : int -> t
(** [null_arr len] is an array of null elements of length [len]. *)

module Syntax : sig
  val ( .:[] ) : t -> int -> Jv.t
  val ( .:[]<- ) : t -> int -> Jv.t -> unit
end

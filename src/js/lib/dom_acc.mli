type t = Jv.t
(** A JavaScript array *)

val create : unit -> t
(** A new, empty accumulator *)

val add_entry : t -> float -> int -> float -> unit 
(** [add_entry dom_acc ts domain_id value] adds a new entry to the accumulator [dom_acc]
    at timestamp [ts] with a value [value] for [domain_id]. *) 
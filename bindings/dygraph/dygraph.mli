open Brr

type data

val data_of_string : string -> data
val data_of_array : float array array -> data
val data_of_jv : Jv.t -> data

type opts

val opts :
  ?draw_points:bool ->
  ?show_roller:bool ->
  ?value_range:float * float ->
  ?file:data ->
  ?legend:[ `Always | `Follow ] ->
  ?labels:string list ->
  ?title:string ->
  ?ylabel:string ->
  ?connect_separated_points:bool ->
  unit ->
  opts

val opts_to_jv : opts -> Jv.t
val opts_of_jv : Jv.t -> opts

type t
(** A dynamic, interactive graphs *)

val create : ?opts:opts -> [ `El of El.t | `Id of string ] -> data -> t
(** [create ~opts el data] makes a new dynamic, interactive graph using the [data]
    provided along with the optional [opts] and either for a specific element or 
    in an element with the specified [id]. *)

val update_opts : t -> opts -> unit
(** [update_opts graph opts] changes various properties of the graph. *)

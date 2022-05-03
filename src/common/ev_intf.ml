module type Json = sig
  type t
  (** The type your parser uses to represent a parsed JSON object. *)

  val find : t -> string list -> t option

  val to_string : t -> (string, [ `Msg of string ]) result
  (** Convert the JSON to a string. *)

  val string : string -> t
  (** Create a JSON string. *)

  val to_float : t -> (float, [ `Msg of string ]) result
  (** Convert the JSON to a float. *)

  val float : float -> t
  (** Converts a float to JSON *)

  val to_int : t -> (int, [ `Msg of string ]) result
  (** Convert the JSON to an integer *)

  val int : int -> t
  (** Converts an integer to JSON *)

  val to_list : (t -> 'a) -> t -> ('a list, [ `Msg of string ]) result
  (** [to_list f] converts the JSON array to a list and applies [f] to each
      element to convert them too. *)

  val list : ('a -> t) -> 'a list -> t
  (** Make a JSON array from a list *)

  val to_array : (t -> 'a) -> t -> ('a array, [ `Msg of string ]) result
  (** Like {!to_list} except to an array. *)

  val array : ('a -> t) -> 'a array -> t
  (** Like {!list} except for OCaml arrays *)

  val obj : (string * t) list -> t
  (** A JSON object from an association list *)

  val null : t
  (** Null value *)

  val is_null : t -> bool
  (** Test for null *)
end

module type S = sig
  type json

  type t =
    | System of
        [ `Counter of system_common * int
        | `Phase of system_common
        | `Lifecycle of system_common ]
    | Eio of string

  and system_common = { name : string; ts : float; domain_id : int }

  val to_json : t -> json
  val of_json : json -> t
end

module type Ev = sig
  module type S = S

  module Make (J : Json) : S with type json = J.t
end

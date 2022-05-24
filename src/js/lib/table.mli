open Brr
open Brr_lwd

module type Row = sig
  type t

  module Id : sig
    type t

    val equal : t -> t -> bool
  end

  val header : El.t
  val to_row : t -> Elwd.t option
  val id : t -> Id.t
end

module Make (R : Row) : sig
  type row = R.t
  type t

  val create : unit -> t
  val prepend : row -> t -> unit
  val find : R.Id.t -> t -> row option
  val update : R.Id.t -> t -> (row option -> row) -> unit
  val to_table : t -> Elwd.t Lwd.t
end

open Brr
open Brr_lwd

module type Row = sig
    type t

    val header : El.t

    val to_row : t -> Elwd.t option
end

module Make (R : Row) : sig 
    type row = R.t

    type t

    val create : unit -> t

    val prepend : row -> t -> unit

    val to_table : t -> Elwd.t Lwd.t
end
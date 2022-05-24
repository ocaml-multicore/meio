open Brr
open Brr_lwd
open Let_syntax

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

module Make (R : Row) = struct
  type row = R.t
  type t = R.t Lwd_table.t

  let create () = Lwd_table.make ()
  let prepend row t = Lwd_table.prepend' t row

  let find id (t : t) =
    let first = Lwd_table.first t in
    let rec lookup = function
      | Some row -> (
          match Lwd_table.get row with
          | Some v' ->
              if R.Id.equal (R.id v') id then Some v'
              else lookup (Lwd_table.next row)
          | None -> None)
      | None -> None
    in
    lookup first

  let update id (t : t) f =
    let first = Lwd_table.first t in
    let rec aux = function
      | Some row -> (
          match Lwd_table.get row with
          | Some v' ->
              if R.Id.equal (R.id v') id then Lwd_table.set row (f (Some v'))
              else aux (Lwd_table.next row)
          | None -> Lwd_table.prepend' t (f None))
      | None -> Lwd_table.prepend' t (f None)
    in
    aux first

  let reducer _row row =
    match R.to_row row with
    | Some el -> Lwd_seq.element el
    | None -> Lwd_seq.empty

  let to_table (tbl : t) =
    let t =
      let+ elements = Lwd_table.map_reduce reducer Lwd_seq.monoid tbl in
      Lwd_seq.to_list elements |> Lwd_seq.of_list
    in
    Elwd.div
      ~at:[ `P (At.class' (Jstr.v "table-container")) ]
      [
        `R
          (Elwd.table
             ~at:[ `P (At.class' (Jstr.v "syst")) ]
             [ `P R.header; `R (Elwd.tbody [ `S t ]) ]);
      ]
end

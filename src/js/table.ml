open Brr
open Brr_lwd
open Let_syntax

module type Row = sig
  type t

  val header : El.t

  val to_row : t -> Elwd.t option
end

module Make (R : Row) = struct
  type row = R.t
  type t = R.t Lwd.t Lwd_table.t

  let create () = 
    Lwd_table.make ()

  let reducer _row row =
    Lwd.map row ~f:(fun row ->
      match R.to_row row with
       | Some el -> Lwd_seq.element el
       | None -> Lwd_seq.empty
    )

  let prepend row t = Lwd_table.prepend' t (Lwd.pure row)

  let el_monoid = Lwd.return Lwd_seq.empty, fun a b -> Lwd.map2 a b ~f:Lwd_seq.concat

  let to_table tbl =
    let t =
      let+ elements = 
        Lwd.join @@
        Lwd_table.map_reduce reducer el_monoid tbl
      in
        Lwd_seq.to_list elements 
        |> Lwd_seq.of_list
    in
      Elwd.div ~at:[ `P (At.class' (Jstr.v "table-container")) ] [
        `R (Elwd.table ~at:[ `P (At.class' (Jstr.v "syst"))] [
          `P R.header;
          `R (Elwd.tbody [ `S t ])
        ])
      ]
end
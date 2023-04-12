val prev_now : (int64 * int64) Lwd.var
val set_prev_now : int64 -> unit
val toggle_fold_selected : Task_tree.t -> unit
val set_selected : Task.t list -> [< `Next | `Prev ] -> unit

val root :
  Sort.t Lwd.t -> Nottui.ui Lwd.t * (int * int * int * Task.t list) option Lwd.t

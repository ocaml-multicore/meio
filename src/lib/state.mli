val tasks : Task_tree.t

val add_tasks :
  id:int ->
  parent_id:int ->
  domain:int ->
  Runtime_events.Timestamp.t ->
  Eio_runtime_events.event ->
  unit

val update_loc : int -> string -> unit
val update_logs : int -> string -> unit
val update_name : int -> string -> unit
val switch_to : id:int -> domain:'a -> Runtime_events.Timestamp.t -> unit
val set_parent : child:int -> parent:int -> Runtime_events.Timestamp.t -> unit
val resolved : int -> Runtime_events.Timestamp.t -> unit
val terminated : Unix.process_status -> int64 -> unit

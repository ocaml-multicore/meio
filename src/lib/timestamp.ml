(* Returns the current value of a counter that increments once per nanosecond. *)
external current : unit -> int64 = "caml_eio_time_counter"

module H = Hdr_histogram
module Ts = Runtime_events.Timestamp

val init : unit -> H.t * ('a -> Ts.t -> 'b -> unit) * ('a -> Ts.t -> 'b -> unit)
val ui' : H.t -> int64 -> Nottui.ui
val ui : H.t -> int64 Lwd.var -> Nottui.ui Lwd.t

val table : string Lwd_table.t
val reporter : unit -> Logs.reporter
val poll : unit -> unit

type 'a log =
  ((?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) ->
  unit

val info : 'a log
val warn : 'a log
val debug : 'a log

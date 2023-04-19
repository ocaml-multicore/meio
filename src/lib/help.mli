val footer :
  Sort.t Lwd.t ->
  [> `Gc | `Help | `Main | `Task | `Logs ] Lwd.t ->
  Nottui.ui Lwd.t

val help : Nottui.ui

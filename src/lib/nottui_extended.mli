module Ui = Nottui.Ui

val scroll_area :
  ?direction:[ `Both | `Horizontal | `Vertical ] ->
  ?offset:int * int ->
  Ui.t Lwd.t ->
  Ui.t Lwd.t

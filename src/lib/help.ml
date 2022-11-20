open Nottui
module W = Nottui_widgets

let help : ui =
  Ui.hcat
    [
      W.string
        {| 
    |\     /| 
    | \___/ |   meio 
    | .   . |   A console for Eio programs.
    |___Y___|
    
    This is how you use meio.

    1. Find a program that uses Eio.
    2. Run meio "<prog> <arg1> ..."
    3. That's it!
    |};
    ]

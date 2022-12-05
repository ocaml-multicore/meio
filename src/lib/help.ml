open Nottui
module W = Nottui_widgets

let key_help c msg =
  W.fmt ~attr:Notty.A.(bg (rgb ~r:4 ~g:2 ~b:0)) "[%c] %s" c msg

let footer =
  [
    key_help 'h' "help";
    Ui.space 5 0;
    key_help 'm' "main page";
    Ui.space 5 0;
    key_help 'g' "GC latencies";
  ]

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

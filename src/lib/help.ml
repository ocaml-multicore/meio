open Nottui
module W = Nottui_widgets

let attr_base = Notty.A.(bg (rgb ~r:4 ~g:2 ~b:0))
let space = W.string ~attr:attr_base "  "

let key_help ?(attr = Notty.A.empty) c msg =
  let attr = Notty.A.(attr_base ++ attr) in
  W.fmt ~attr "[%c] %s" c msg

let footer sort screen =
  let open Lwd_infix in
  let$ sort = sort and$ screen = screen in
  let attr sc = if sc = screen then Notty.A.(st bold) else Notty.A.empty in
  Ui.hcat
    [
      key_help ~attr:(attr `Help) 'h' "help";
      space;
      key_help ~attr:(attr `Main) 'm' "main page";
      space;
      key_help ~attr:(attr `Gc) 'g' "GC latencies";
      space;
      key_help ~attr:(attr `Task) 'e' "Fiber info";
      space;
      key_help ~attr:(attr `Logs) 'l' "Logs";
      space;
      key_help 's' ("Sort " ^ Sort.to_string sort);
      space;
      key_help 'q' "Quit";
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

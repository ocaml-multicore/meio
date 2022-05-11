(* We send mirage trace viewer events over the websocket
   to the client in order to render the graph. This is in 
   contrast to the mirage-trace-viewer-js CLI which can only
   generate the viewer after the fact. *)
open Mirage_trace_viewer

let load src =
  let ba = Mtv_unix.load src in
  let vat = Mtv_ctf_loader.from_bigarray ba |> Mtv_thread.of_events in
  let view = Mtv_view.make ~vat ~view_width:200. ~view_height:200. in
  Marshal.to_string view [ Compat_32 ]
  







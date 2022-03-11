open Brr
open Brr_io
open Brr_lwd
open Let_syntax

module Events = Events
module Table = Table


module Runtime_row = struct
  type t = {
    name : string;
    ts : float;
    domain_id : int;
  }

  let header = El.thead [ El.tr [ El.td [ El.txt' "Event Name" ]; El.td [ El.txt' "TS" ];  El.td [ El.txt' "Domain ID" ] ] ]

  let to_row row = 
    Some (El.tr ~at:[ At.tabindex 0 ] [ 
      El.td [ 
        El.txt' row.name]; 
        El.(td [ txt' (string_of_float row.ts) ]); 
        El.(td [ txt' (string_of_int row.domain_id) ])
    ])
end

(* module Counter_row = struct
  type t = {
    name : string;
    ts : float;
    domain_id : int;
    value : int;
  }

  let header = El.thead [ 
    El.tr [ 
      El.td [ El.txt' "name" ];
      El.td [ El.txt' "ts" ];
      El.td [ El.txt' "domain_id" ];
      El.td [ El.txt' "value" ]
    ]]

  let to_row row = 
    Some (El.tr ~at:[ At.tabindex 0 ] [ 
      El.td [ 
        El.txt' row.name]; 
        El.(td [ txt' (string_of_float row.ts) ]); 
        El.(td [ txt' (string_of_int row.domain_id) ]);
        El.(td [ txt' (string_of_int row.value) ])
    ])
end *)

module Runtime_table = Table.Make(Runtime_row)
let runtime_table = Runtime_table.create ()
(* module Counter_table = Table.Make(Counter_row)
let counter_table = Counter_table.create () *)

let or_raise = function
  | Ok v -> v 
  | Error err -> 
    Console.log [ Jv.Error.stack err ];
    failwith "err"

open Let_syntax

let graph =
  let opts = 
    Dygraph.opts 
    ~draw_points:true 
    ~show_roller:true 
    ~labels:[ "ts"; "dom0"; "dom1" ]
    ~title:"Requested Major Allocations"
    ~legend:`Always
    ~ylabel:"words"
    ~connect_separated_points:true () 
  in
  Dygraph.create ~opts (`Id "g") (Dygraph.data_of_array [| [| 0.; 0.; 0. |] |])

let dom_acc = Dom_acc.create ()

let ui =
  let rtable = Runtime_table.to_table runtime_table in
  (* let ctable = Counter_table.to_table counter_table in *)
  Elwd.div [ `R rtable; ]

let connect () = 
  let on_message msg =
    let msg = Message.Ev.data @@ Ev.as_type msg in
    let json = Brr.Json.decode msg |> or_raise in
    match Events.of_json json with
      | Events.System (`Phase { name; ts; domain_id }) ->
        Runtime_table.prepend { Runtime_row.name; ts; domain_id } runtime_table
      | Events.System (`Lifecycle { name; ts; domain_id }) ->
        Runtime_table.prepend { Runtime_row.name; ts; domain_id } runtime_table
      | Events.System (`Counter ({ name; ts; domain_id }, value)) ->
        (* Counter_table.prepend { Counter_row.name; ts; domain_id; value } counter_table; *)
        if name = "C_REQUEST_MAJOR_ALLOC_SHR" then begin 
          Dom_acc.add_entry dom_acc ts domain_id (float_of_int value);
          let file = Dygraph.data_of_jv dom_acc in
          Dygraph.update_opts graph (Dygraph.opts ~file ())
        end
      | e -> 
        Console.log [ Console.str "Non-system Event"; e ] 
  in 
  let ws = Websocket.create (Jstr.v "ws://localhost:8080/websocket") in
  Ev.listen Message.Ev.message on_message (Websocket.as_target ws)

let () =
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    let _ : int =
      G.request_animation_frame @@ fun _ ->
      let _ui = Lwd.quick_sample ui in
      ()
    in
    ()
  in
  let on_load _ =
    match Document.find_el_by_id G.document (Jstr.v "container") with
      | None -> failwith "No element with id `container` found!"
      | Some el ->
        El.append_children el [ Lwd.quick_sample ui ];
        Lwd.set_on_invalidate ui on_invalidate;
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window);
  connect ();
  ()
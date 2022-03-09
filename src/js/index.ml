open Brr
open Brr_io
open Brr_lwd

module Events = Events

module Let_syntax = struct
  let (let+) v f = Lwd.map v ~f
  let (and+) = Lwd.pair

  let (let*) v f = Lwd.bind v ~f
  let (and*) = Lwd.pair
end

let or_raise = function
  | Ok v -> v 
  | Error err -> 
    Console.log [ Jv.Error.stack err ];
    failwith "err"

open Let_syntax

type row = {
  name : string;
  ts : float;
}

let table = 
  Lwd_table.make ()

let row init = 
  let ts = Lwd.var init in
  Lwd.map (Lwd.get ts) ~f:(fun ts -> { name = "ABC"; ts })

let generate n =
  let entries = List.init n (fun i -> row @@ float_of_int i) in
  List.iter (fun t -> Lwd_table.append' table t) entries

let reducer _row row =
  Lwd.map row ~f:(fun row -> Lwd_seq.element @@ El.tr ~at:[ At.tabindex 0 ] [ El.td [ El.txt' row.name]; (El.(td [ txt' (string_of_float row.ts) ])) ])

let el_monoid = Lwd.return Lwd_seq.empty, fun a b -> Lwd.map2 a b ~f:Lwd_seq.concat

let mimic_new_data () =
  Lwd_table.prepend' table (row 10.)

let button = 
  let bt = El.button [ El.txt' "ADDDDDDDD" ] in
  Ev.listen Ev.click (fun _ -> for _ = 0 to 100 do mimic_new_data () done) (El.as_target bt);
  bt

let ui =
  let t =
    let+ elements = 
      Lwd.join @@
      Lwd_table.map_reduce reducer el_monoid table
    in
      Lwd_seq.to_list elements 
      |> List.filteri (fun i _ -> i < 10) 
      |> Lwd_seq.of_list
  in
  let table = 
    Elwd.table [
      `P (El.thead [ El.tr [ El.td [ El.txt' "name" ]; El.td [ El.txt' "ts" ] ] ]);
      `R (Elwd.tbody [ `S t ])
    ]
  in 
  Elwd.div [ `P button; `R table; ]

let connect () = 
  Console.log [ "CONNECT" ];
  let on_message msg =
    Console.log [ Jstr.v "Message"; msg ];
    let msg = Message.Ev.data @@ Ev.as_type msg in
    let json = Brr.Json.decode msg |> or_raise in
    match Events.of_json json with
      | Events.System { name; ts } ->
        Console.log [ Jstr.v name; ts ];
        Lwd_table.prepend' table (Lwd.pure { name; ts })
      | _ -> 
        Console.log [ Console.str "YIKES" ] 
  in 
  let ws = Websocket.create (Jstr.v "ws://localhost:8080/websocket") in
  Ev.listen Message.Ev.message on_message (Websocket.as_target ws)

let () =
  generate 10;
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
    El.append_children (Document.body G.document) [Lwd.quick_sample ui];
    Lwd.set_on_invalidate ui on_invalidate;
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window);
  connect ();
  ()
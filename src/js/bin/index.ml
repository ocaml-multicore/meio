open Brr
open Brr_io
open Brr_lwd
open Ec_js

module Runtime_row = struct
  type t = { name : string; ts : float; domain_id : int }

  let id { name; _ } = name

  module Id = struct
    type t = string

    let equal = String.equal
  end

  let header =
    El.thead
      [
        El.tr
          [
            El.td [ El.txt' "Event Name" ];
            El.td [ El.txt' "TS" ];
            El.td [ El.txt' "Domain ID" ];
          ];
      ]

  let to_row row =
    Some
      (El.tr
         ~at:[ At.tabindex 0 ]
         [
           El.td [ El.txt' row.name ];
           El.(td [ txt' (string_of_float row.ts) ]);
           El.(td [ txt' (string_of_int row.domain_id) ]);
         ])
end
(*
   module Counter_row = struct
     type t = { name : string; ts : float; domain_id : int; value : int }

     module Id = struct
       type t = string * int

       let equal (s, id) (s', id') = String.equal s s' && Int.equal id id'
     end

     let id { name; domain_id; _ } = (name, domain_id)

     let header =
       El.thead
         [
           El.tr
             [
               El.td [ El.txt' "name" ];
               El.td [ El.txt' "ts" ];
               El.td [ El.txt' "domain_id" ];
               El.td [ El.txt' "value" ];
             ];
         ]

     let to_row row =
       Some
         (El.tr
            ~at:[ At.tabindex 0 ]
            [
              El.td [ El.txt' row.name ];
              El.(td [ txt' (string_of_float row.ts) ]);
              El.(td [ txt' (string_of_int row.domain_id) ]);
              El.(td [ txt' (string_of_int row.value) ]);
            ])
   end *)

let or_raise = function
  | Ok v -> v
  | Error err ->
      Console.log [ Jv.Error.stack err ];
      failwith "err"

let make_labels n_domains =
  "ts" :: List.init n_domains (fun id -> "dom" ^ string_of_int id)

let make_dygraph title =
  let opts =
    Dygraph.opts ~draw_points:true ~show_roller:true ~labels:(make_labels 1)
      ~title ~legend:`Always ~ylabel:"words" ~connect_separated_points:true ()
  in
  Dygraph.create ~opts (`Id title) (Dygraph.data_of_array [| [| 0.; 0. |] |])

module Runtime_table = Table.Make (Runtime_row)

let add_entry t k v =
  try Hashtbl.(replace t k @@ (find t k +. v))
  with Not_found -> Hashtbl.add t k v

let phase_table = Runtime_table.create ()
let lifecycle_table = Runtime_table.create ()
let counter_arrays : (string, Jarr.t * Dygraph.t) Hashtbl.t = Hashtbl.create 8

module Counter_array = struct
  open Jarr.Syntax

  type t = Jarr.t

  let create () = Jarr.create 0

  let update_after_existing ~t ~domain_id idx =
    let v = t.:[idx].:[domain_id + 1] in
    for i = idx + 1 to Jarr.length t - 1 do
      t.:[i].:[domain_id + 1] <- v
    done

  let update_after_inserting ~t ~domain_id idx =
    let length = Jarr.length t in
    let v = t.:[idx].:[domain_id + 1] in
    for i = idx + 1 to length - 1 do
      t.:[i].:[domain_id + 1] <- v
    done

  let grow_domains t n =
    let length = Jarr.length t in
    let diff = n - (Jarr.length t.:[0] - 1) in
    for i = 0 to length - 1 do
      for _j = 0 to diff - 1 do
        Jarr.push t.:[i] (Jv.of_float 0.)
      done
    done

  let add_entry t ts domain_id v =
    (* Ensure there's enough columns for the number of domains. *)
    let length = Jarr.length t in
    match length with
    | 0 ->
        (* This corresponds to (1) above. *)
        let a = Jarr.null_arr (domain_id + 1) in
        a.:[0] <- Jv.of_float ts;
        a.:[domain_id + 1] <- Jv.of_float v;
        Jarr.push t a
    | length -> (
        let n_domains = Jarr.length t.:[0] - 1 in
        if n_domains < domain_id + 1 then grow_domains t (domain_id + 1) else ();
        match ts > Jv.to_float t.:[length - 1].:[0] with
        | true ->
            (* This corresponds to (2) above. *)
            let a = Jarr.copy t.:[length - 1] in
            a.:[0] <- Jv.of_float ts;
            a.:[domain_id + 1] <-
              Jv.of_float (Jv.to_float a.:[domain_id + 1] +. v);
            Jarr.push t a
        | false ->
            let added_new_entry = ref false in
            let idx = ref 0 in
            while (not !added_new_entry) && !idx < length do
              if Jv.to_float t.:[!idx].:[0] = ts then (
                (* This corresponds to (3) above. *)
                (* TODO: ASSERT NULL? Invariant: could we have received a timestamp after
                    [ts] already for this particular domain? *)
                t.:[!idx].:[domain_id + 1] <-
                  Jv.of_float (Jv.to_float t.:[!idx - 1].:[domain_id + 1] +. v);
                update_after_existing ~t ~domain_id !idx;
                added_new_entry := true)
              else if
                !idx > 0
                && ts > Jv.to_float t.:[!idx].:[0]
                && ts < Jv.to_float t.:[!idx + 1].:[0]
              then (
                (* This corresponds to (4) above. *)
                let a = Jarr.copy t.:[!idx] in
                a.:[0] <- Jv.of_float ts;
                let prev =
                  let p = t.:[!idx].:[domain_id + 1] in
                  if Jv.is_null p then 0. else Jv.to_float p
                in
                a.:[domain_id + 1] <- Jv.of_float (prev +. v);
                Jarr.insert (!idx + 1) t a;
                update_after_inserting ~t ~domain_id (!idx + 1);
                added_new_entry := true)
              else ();
              incr idx
            done)
end

let add_graph_div name =
  let id = Jstr.v name in
  match Document.find_el_by_id G.document id with
  | Some _ -> ()
  | None ->
      let div = El.div ~at:[ At.class' (Jstr.v "graph"); At.id id ] [] in
      El.set_at (Jstr.v "style") (Some (Jstr.v "width: 720px")) div;
      El.append_children
        (Option.get @@ Document.find_el_by_id G.document (Jstr.v "graphs"))
        [ div ]

let update_counter_array name ts domain_id value =
  let open Jarr.Syntax in
  match Hashtbl.find_opt counter_arrays name with
  | Some (arr, dygraph) ->
      Counter_array.add_entry arr ts domain_id value;
      let file = Dygraph.data_of_jv arr in
      Dygraph.update_opts dygraph
        (Dygraph.opts ~file ~labels:(make_labels (Jarr.length arr.:[0] - 1)) ())
  | None ->
      let arr = Counter_array.create () in
      Counter_array.add_entry arr ts domain_id value;
      add_graph_div name;
      let dygraph = make_dygraph name in
      Hashtbl.add counter_arrays name (arr, dygraph);
      let file = Dygraph.data_of_jv arr in
      Dygraph.update_opts dygraph
        (Dygraph.opts ~file ~labels:(make_labels (Jarr.length arr.:[0] - 1)) ())

let ui =
  let ptable = Runtime_table.to_table phase_table in
  let ltable = Runtime_table.to_table lifecycle_table in
  Elwd.div
    [
      `P (El.h3 [ El.txt' "Phases" ]);
      `R ptable;
      `P (El.h3 [ El.txt' "Lifecycle Events" ]);
      `R ltable;
    ]

let connect () =
  let on_message msg =
    let msg = Message.Ev.data @@ Ev.as_type msg in
    let json = Brr.Json.decode msg |> or_raise in
    match Events.of_json json with
    | Events.System (`Phase ({ name; ts; domain_id }, _)) ->
        Runtime_table.prepend { Runtime_row.name; ts; domain_id } phase_table
    | Events.System (`Lifecycle { name; ts; domain_id }) ->
        Runtime_table.prepend
          { Runtime_row.name; ts; domain_id }
          lifecycle_table
    | Events.System (`Counter ({ name; domain_id; ts }, value)) ->
        (* Update the arrays for the graphs *)
        update_counter_array name ts domain_id (float_of_int value)
    | _ -> ()
    (* | e -> Console.log [ Console.str "Non-system Event"; e ] *)
  in
  let ws = Websocket.create (Jstr.v "ws://localhost:8080/websocket") in
  Ev.listen Message.Ev.message on_message (Websocket.as_target ws)

let () =
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    let (_ : int) =
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
        Lwd.set_on_invalidate ui on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window);
  connect ();
  ()

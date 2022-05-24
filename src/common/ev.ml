module type S = Ev_intf.S

(* We use a slightly custom format for the JSON representation
   of the data we send. By extracting the JSON codec we can allow
   the JavaScript code to use the built-in browser tools for encoding
   and decoding the data which is *much* faster. *)
module Make (J : Ev_intf.Json) = struct
  type json = J.t

  type t =
    | System of
        [ `Counter of system_common * int
        | `Phase of system_common * phase
        | `Lifecycle of system_common ]
    | Eio of string

  and system_common = { name : string; ts : float; domain_id : int }
  and phase = Begin | End

  let phase_to_string = function Begin -> "Begin" | End -> "End"

  let phase_of_string = function
    | "Begin" -> Ok Begin
    | "End" -> Ok End
    | _ -> Error (`Msg "Unknown phase")

  let to_json = function
    | System (`Phase ({ name; ts; domain_id }, phase)) ->
        J.obj
          [
            ("kind", J.string "system-phase");
            ("name", J.string name);
            ("ts", J.float ts);
            ("domain_id", J.int domain_id);
            ("phase", J.string @@ phase_to_string phase);
          ]
    | System (`Lifecycle { name; ts; domain_id }) ->
        J.obj
          [
            ("kind", J.string "system-lifecycle");
            ("name", J.string name);
            ("ts", J.float ts);
            ("domain_id", J.int domain_id);
          ]
    | System (`Counter ({ name; ts; domain_id }, value)) ->
        J.obj
          [
            ("kind", J.string "system-counter");
            ("name", J.string name);
            ("ts", J.float ts);
            ("domain_id", J.int domain_id);
            ("value", J.int value);
          ]
    | Eio s -> J.obj [ ("kind", J.string "eio"); ("data", J.string s) ]

  let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m

  let of_json j =
    let system_common j =
      let name = J.find j [ "name" ] |> Option.get |> J.to_string |> or_raise in
      let ts = J.find j [ "ts" ] |> Option.get |> J.to_float |> or_raise in
      let domain_id =
        J.find j [ "domain_id" ] |> Option.get |> J.to_int |> or_raise
      in
      { name; ts; domain_id }
    in
    match J.find j [ "kind" ] with
    | None -> invalid_arg "Invalid JSON, should have a `kind` field"
    | Some t -> (
        match J.to_string t |> or_raise with
        | "system-phase" ->
            let phase =
              J.find j [ "phase" ] |> Option.get |> J.to_string |> fun v ->
              Result.bind v phase_of_string |> or_raise
            in
            System (`Phase (system_common j, phase))
        | "system-lifecycle" -> System (`Lifecycle (system_common j))
        | "system-counter" ->
            let value =
              J.find j [ "value" ] |> Option.get |> J.to_int |> or_raise
            in
            System (`Counter (system_common j, value))
        | "eio" ->
            let data =
              J.find j [ "data" ] |> Option.get |> J.to_string |> or_raise
            in
            Eio data
        | s -> failwith ("Unknown kind of event: " ^ s))
end

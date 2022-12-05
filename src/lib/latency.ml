(* Latency code from Olly/Runtime_events_tools

   Copyright (c) 2022 Sadiq Jaffer <sadiq@toao.com>

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

open Nottui
module W = Nottui_widgets
module H = Hdr_histogram
module Ts = Runtime_events.Timestamp

let init () =
  let current_event = Hashtbl.create 13 in
  let hist =
    H.init ~lowest_discernible_value:10 ~highest_trackable_value:10_000_000_000
      ~significant_figures:3
  in
  let runtime_begin ring_id ts phase =
    match Hashtbl.find_opt current_event ring_id with
    | None -> Hashtbl.add current_event ring_id (phase, Ts.to_int64 ts)
    | _ -> ()
  in
  let runtime_end ring_id ts phase =
    match Hashtbl.find_opt current_event ring_id with
    | Some (saved_phase, saved_ts) when saved_phase = phase ->
        Hashtbl.remove current_event ring_id;
        let latency = Int64.to_int (Int64.sub (Ts.to_int64 ts) saved_ts) in
        assert (H.record_value hist latency)
    | _ -> ()
  in
  (hist, runtime_begin, runtime_end)

let percentiles =
  [ 25.0; 50.0; 60.0; 70.0; 80.0; 90.0; 95.0; 99.0; 99.9; 99.99 ]

let max_list =
  List.fold_left (fun max v -> if Int64.compare v max > 0 then v else max) 0L

let ns_span i = Fmt.(to_to_string uint64_ns_span (Int64.of_int i))

let ui' h ts =
  let percentiles =
    let percentiles =
      List.map (fun p -> (p, H.value_at_percentile h p)) percentiles
    in
    List.map (fun (p, i) -> W.fmt "%.2f%% %s" p (ns_span i)) percentiles
  in
  let stats =
    Ui.hcat
      [
        W.fmt
          ~attr:Notty.A.(st italic)
          "max: %s, min: %s, std: %s, mean: %s"
          (ns_span @@ H.max h)
          (ns_span @@ H.min h)
          (ns_span @@ int_of_float @@ H.stddev h)
          (ns_span @@ int_of_float @@ H.mean h);
      ]
  in
  let title =
    W.fmt
      ~attr:Notty.A.(st bold ++ fg green)
      "Garbage Collection Latencies (program duration: %a)" Fmt.uint64_ns_span
      ts
  in
  Ui.vcat (title :: stats :: percentiles)

let ui h ts = Lwd.map ~f:(fun ts -> ui' h ts) (Lwd.get ts)

(* A per-domain accumulator for counters. Here we store the information for the
   dygraphs on a per-domain basis updating the numbers based on the timestamp and
   new information. Although untested, the implementation is imperative using a mutable
   JavaScript array which I suspect will offer better performance than using OCaml 
   arrays and converting them. *)
open Jarr.Syntax
open Brr

type t = Jarr.t

let create () = Jarr.create 0 

(* Adding an entry:
   1. If the dom_acc is empty then we can push a new value into the array.
   2. If the entry's timestamp is newer than the last entry in [dom_acc] then we make a copy
      of the last entry and add on the counter's value to the correct domain value.
   3. If the timestamp is already present then we must update that entry and all entries going
      forward from that point with the accumulated value.
   4. If the timestamp falls inbetween two entries already in [dom_acc] then this is similar to
      (3) except we'll insert the value into array.  *)

(* This function is for case (3) *)
let update_after_existing ~dom_acc ~domain_id idx =
  (* (assert (Jv.is_null (arr.:[idx].:[domain_id]))) *)
  let v = dom_acc.:[idx].:[domain_id + 1] in 
  for i = idx + 1 to Jarr.length dom_acc - 1 do
    dom_acc.:[i].:[domain_id + 1] <- v;
    Console.log [ i; dom_acc ]
  done

(* This function is for case (4) *)
let update_after_inserting ~dom_acc ~domain_id idx =
  let length = Jarr.length dom_acc in
  let v = dom_acc.:[idx].:[domain_id + 1] in 
  Console.log [ Jstr.v "INSERT UP"; v ];
  for i = idx + 1 to length - 1 do
    dom_acc.:[i].:[domain_id + 1] <- v;
  done

(* TODO: This logic needs checked! *)
let add_entry dom_acc ts domain_id v =
  let length = Jarr.length dom_acc in
  match length with
    | 0 -> 
      (* This corresponds to (1) above. *)
      (* TODO: Agnostic to number of domains *)
      let a = Jarr.null_arr 3 in
      a.:[0] <- Jv.of_float ts;
      a.:[domain_id + 1] <- Jv.of_float v;
      Jarr.push dom_acc a
    | length ->
      match ts > Jv.to_float dom_acc.:[length - 1].:[0] with
      | true -> 
        (* This corresponds to (2) above. *)
        let a = Jarr.copy dom_acc.:[length - 1] in
        a.:[0] <- Jv.of_float ts;
        a.:[domain_id + 1] <- Jv.of_float (Jv.to_float a.:[domain_id + 1] +. v);
        Jarr.push dom_acc a
      | false ->
        let added_new_entry = ref false in
        let idx = ref 0 in
        while ( not (!added_new_entry) && !idx < length ) do
          if Jv.to_float dom_acc.:[!idx].:[0] = ts then begin
            (* This corresponds to (3) above. *)
            (* TODO: ASSERT NULL? Invariant: could we have received a timestamp after 
               [ts] already for this particular domain? *)
               dom_acc.:[!idx].:[ domain_id + 1 ] <- Jv.of_float (Jv.to_float dom_acc.:[!idx - 1].:[domain_id + 1] +. v);
            update_after_existing ~dom_acc ~domain_id !idx;
            added_new_entry := true
          end else if (!idx > 0 && ts > Jv.to_float dom_acc.:[!idx].:[0] && ts < Jv.to_float dom_acc.:[!idx + 1].:[0]) then begin
            (* This corresponds to (4) above. *)
            let a = Jarr.copy dom_acc.:[!idx] in
            a.:[0] <- Jv.of_float ts;
            let prev = 
              let p = dom_acc.:[!idx].:[domain_id + 1] in
              if Jv.is_null p then 0. else Jv.to_float p
            in
            a.:[domain_id + 1] <- Jv.of_float (prev +. v);
            Jarr.insert (!idx + 1) dom_acc a;
            update_after_inserting ~dom_acc ~domain_id (!idx + 1);
            added_new_entry := true
          end else begin
            ()
          end;
          incr idx
        done
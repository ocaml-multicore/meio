type t = Domain | Id | Busy | Tree | No_sort

type sort =
  | V : {
      prepare : ((Task.t -> unit) -> unit) -> 'a;
      compare : 'a -> Task.t -> Task.t -> int;
    }
      -> sort

let next = function
  | Busy -> Tree
  | Tree -> No_sort
  | No_sort -> Id
  | Id -> Domain
  | Domain -> Busy

let to_string = function
  | Busy -> "Busy"
  | Tree -> "Tree"
  | No_sort -> "No_sort"
  | Id -> "Id"
  | Domain -> "Domain"

let state_to_int t = match t.Task.status with Resolved _ -> 0 | _ -> 1

(* parent => children *)
module Map = Map.Make (Int)

let topo_sort map =
  let rec topo_sort_dfs ~order ~root ~depth acc =
    match Map.find_opt root map with
    | None -> (acc, order)
    | Some children ->
        let len = List.length children in

        children
        |> List.sort (fun (_, t1) (_, t2) ->
               -Int.compare (state_to_int t1) (state_to_int t2))
        |> List.fold_left
             (fun ((acc, order), n) (id, payload) ->
               let depth = (n = len - 1) :: depth in
               payload.Task.depth <- depth;
               ( topo_sort_dfs ~order:(order + 1) ~root:id ~depth
                   (Map.add payload.Task.id order acc),
                 n + 1 ))
             ((acc, order), 0)
        |> fst
  in
  topo_sort_dfs ~order:0 ~root:(-1) ~depth:[] Map.empty |> fst

let merge_fn _ b c =
  match (b, c) with
  | None, None -> None
  | Some a, Some b -> Some (a @ b)
  | Some a, None -> Some a
  | None, Some b -> Some b

let prepare iter =
  let map = ref Map.empty in
  iter (fun item ->
      map :=
        Map.merge merge_fn
          (Map.singleton item.Task.parent_id [ (item.id, item) ])
          !map);
  topo_sort !map

let by_state compare =
  V
    {
      compare =
        (fun () t t2 ->
          match Int.compare (state_to_int t) (state_to_int t2) with
          | 0 -> compare t t2
          | v -> v);
      prepare = (fun _ -> ());
    }

let compare = function
  | Busy ->
      by_state @@ fun t t2 ->
      Int64.compare (Task.Busy.total t.busy) (Task.Busy.total t2.busy)
  | Id -> by_state @@ fun t t2 -> Int.compare t.id t2.id
  | Domain -> by_state @@ fun t t2 -> Int.compare t.domain t2.domain
  | Tree ->
      V
        {
          prepare;
          compare =
            (fun map t t2 ->
              -Int.compare (Map.find t.id map) (Map.find t2.id map));
        }
  | _ -> V { prepare = (fun _ -> ()); compare = (fun _ _ _ -> 0) }

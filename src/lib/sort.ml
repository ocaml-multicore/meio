type t = Domain | Id | Busy | Tree

let next = function Busy -> Tree | Tree -> Id | Id -> Domain | Domain -> Busy

let to_string = function
  | Busy -> "Busy"
  | Tree -> "Tree"
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
        let c =
          children
          |> List.sort (fun (_, t1) (_, t2) ->
                 -Int.compare (state_to_int t1) (state_to_int t2))
          |> List.to_seq
        in
        let next =
          Seq.append
            (Seq.map snd c |> Seq.drop 1 |> Seq.map Option.some)
            (Seq.return None)
        in
        Seq.zip c next
        |> Seq.fold_left
             (fun ((acc, order), n) ((id, payload), next) ->
               let depth =
                 (match next with
                 | None -> None
                 | Some { Task.status = Resolved _; _ } -> Some false
                 | _ -> Some true)
                 :: depth
               in
               (*  payload.Task.depth <- depth; *)
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

let by_state compare t t2 =
  match Int.compare (state_to_int t) (state_to_int t2) with
  | 0 -> compare t t2
  | v -> -v

let compare = function
  | Busy ->
      by_state @@ fun t t2 ->
      Int64.compare (Task.Busy.total t.busy) (Task.Busy.total t2.busy)
  | Id -> by_state @@ fun t t2 -> Int.compare t.id t2.id
  | Domain -> by_state @@ fun t t2 -> Int.compare t.domain t2.domain
  | _ -> fun _ _ -> 0

let[@tail_mod_cons] rec merge cmp l1 l2 =
  match (l1, l2) with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      if cmp h1 h2 <= 0 then h1 :: merge cmp t1 l2 else h2 :: merge cmp l1 t2

let merge_sort_monoid : ('a -> 'a -> int) -> 'a list Lwd_utils.monoid =
 fun compare -> ([], merge compare)

type t = Domain | Id | Busy | Tree | No_sort

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

let by_state compare t t2 =
  match Int.compare (state_to_int t) (state_to_int t2) with
  | 0 -> compare t t2
  | v -> v

let compare = function
  | Busy ->
      by_state @@ fun t t2 ->
      Int64.compare (Task.Busy.total t.busy) (Task.Busy.total t2.busy)
  | Id -> by_state @@ fun t t2 -> Int.compare t.id t2.id
  | Domain -> by_state @@ fun t t2 -> Int.compare t.domain t2.domain
  | _ -> fun _ _ -> 0

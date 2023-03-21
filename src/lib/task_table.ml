type sort = Domain | Id | Busy | No_sort

(* Invariant, the lwd_table is always sorted *)
type t = { sort : sort; table : Task.t Lwd_table.t }

let create () = { table = Lwd_table.make (); sort = Busy }

let filter f row =
  let rec loop = function
    | None -> ()
    | Some row ->
        let next = Lwd_table.next row in
        if f row then () else Lwd_table.remove row;
        loop next
  in
  loop row

let map f row =
  let rec loop = function
    | None -> ()
    | Some row ->
        let next = Lwd_table.next row in
        let t = f (Option.get (Lwd_table.get row)) in
        Lwd_table.set row t;
        loop next
  in
  loop row

let iter_with_prev f row =
  let rec loop acc = function
    | None -> ()
    | Some row ->
        let next = Lwd_table.next row in
        let current = Option.get (Lwd_table.get row) in
        f ~prev:acc current;
        (* Lwd_table.set row t; *)
        loop (Some current) next
  in
  loop None row

let find_first f row =
  let rec loop = function
    | None -> None
    | Some row -> (
        match Lwd_table.get row with
        | None -> loop (Lwd_table.next row)
        | Some v -> if f v then Some row else loop (Lwd_table.next row))
  in
  loop row

let remove_by_id { table; _ } id =
  filter
    (fun t ->
      match Lwd_table.get t with
      | Some t -> Int.equal t.Task.id id
      | None -> true)
    (Lwd_table.first table)

let update_loc { table; _ } id info =
  map
    (fun t ->
      if Int.equal t.Task.id id then { t with info = info :: t.info } else t)
    (Lwd_table.first table)

let update_active { table; _ } ~domain ~id =
  map
    (fun t ->
      if Int.equal t.Task.id id && Int.equal t.Task.domain domain then
        { t with active = true; busy = ref 0L :: t.busy }
      else if Int.equal t.Task.domain domain then { t with active = false }
      else t)
    (Lwd_table.first table)

let find_sort_row t v = function
  | Busy ->
      Int64.compare (Task.get_current_busy t) (Task.get_current_busy v) < 0
  | _ -> true

let add ({ table; sort; _ } : t) task =
  let set_selected = Option.is_none @@ Lwd_table.first table in
  let first =
    find_first (fun v -> find_sort_row task v sort) (Lwd_table.first table)
  in
  (* Hmmmm *)
  (match first with
  | None -> Lwd_table.append' table task
  | Some row -> ignore (Lwd_table.before ~set:task row));
  if set_selected then task.selected <- true

let refresh_active_tasks diff ({ table; _ } as t) =
  let rec collect_and_remove acc = function
    | None -> List.rev acc
    | Some row -> (
        match Lwd_table.get row with
        | None -> collect_and_remove acc (Lwd_table.next row)
        | Some (task : Task.t) ->
            let next = Lwd_table.next row in
            let acc =
              if task.active then (
                let busy = List.hd task.busy in
                busy := Int64.add !busy diff;
                Lwd_table.remove row;
                task :: acc)
              else acc
            in
            collect_and_remove acc next)
  in
  let active = collect_and_remove [] (Lwd_table.first table) in
  List.iter (add t) active

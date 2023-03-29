type t = {
  table : Task.t Lwd_table.t;
  by_id : (int, Task.t Lwd_table.row) Hashtbl.t;
  mutable mode : Sort.t;
  mutable compare : Sort.sort;
  mutable active_id : int;
}

let set_sort_mode t mode =
  t.mode <- mode;
  t.compare <- Sort.compare mode

let create mode =
  {
    table = Lwd_table.make ();
    mode;
    compare = Sort.compare mode;
    by_id = Hashtbl.create 10;
    active_id = -1;
  }

let get_row v = Lwd_table.get v |> Option.get

(* bubble sort. we need a sort that is linear when the table is already sorted. *)
let sort { table; compare; by_id; _ } =
  let (V { prepare; compare }) = compare in
  let prep = prepare (fun fn -> Lwd_table.iter fn table) in
  let rec bubble previous value =
    let p = get_row previous in
    let v = get_row value in
    if compare prep p v <= 0 then ()
    else (
      Lwd_table.set previous v;
      Hashtbl.replace by_id v.id previous;
      Lwd_table.set value p;
      Hashtbl.replace by_id p.id value;
      match Lwd_table.prev previous with
      | None -> ()
      | Some ante -> bubble ante previous)
  in
  let rec bubble_loop previous = function
    | None -> ()
    | Some row ->
        bubble previous row;
        bubble_loop row (Lwd_table.next row)
  in
  match Lwd_table.first table with
  | None -> ()
  | Some first -> bubble_loop first (Lwd_table.next first)

let filter t f =
  let rec filter_loop = function
    | None -> ()
    | Some row ->
        let next = Lwd_table.next row in
        (if f row then ()
         else
           let v = get_row row in
           Lwd_table.remove row;
           Hashtbl.remove t.by_id v.Task.id);
        filter_loop next
  in
  filter_loop (Lwd_table.first t.table)

let map t f =
  let rec map_loop = function
    | None -> ()
    | Some row ->
        let next = Lwd_table.next row in
        let t = get_row row in
        let t' = f t in
        if t != t' then Lwd_table.set row t';
        map_loop next
  in
  map_loop (Lwd_table.first t.table)

let iter_with_prev t f =
  let rec iter_wp_loop acc = function
    | None -> ()
    | Some row ->
        let next = Lwd_table.next row in
        let current = get_row row in
        f ~prev:acc current;
        (* Lwd_table.set row t; *)
        iter_wp_loop (Some current) next
  in
  iter_wp_loop None (Lwd_table.first t.table)

let find_first f row =
  let rec ff_loop = function
    | None -> None
    | Some row -> (
        match Lwd_table.get row with
        | None -> ff_loop (Lwd_table.next row)
        | Some v -> if f v then Some row else ff_loop (Lwd_table.next row))
  in
  ff_loop row

let remove_by_id t id =
  filter t (fun t ->
      match Lwd_table.get t with
      | Some t -> Int.equal t.Task.id id
      | None -> true)

let update_loc t id loc =
  map t (fun t ->
      if Int.equal t.Task.id id then { t with loc = loc :: t.loc } else t)

let update_logs { by_id; _ } id logs =
  let row = Hashtbl.find by_id id in
  let t = get_row row in
  Lwd_table.set row { t with logs = logs :: t.logs }

let update_name { by_id; _ } id name =
  let row = Hashtbl.find by_id id in
  let t = get_row row in
  Lwd_table.set row { t with name = name :: t.name }

let update_active ({ by_id; active_id; _ } as t) ~domain:_ ~id ts =
  (match Hashtbl.find_opt by_id active_id with
  | None -> ()
  | Some active_row ->
      let t = get_row active_row in
      let row =
        match t.status with
        | Active start ->
            Task.Busy.add t.busy (Int64.sub ts start);
            { t with status = Paused }
        | _ -> t
      in
      Lwd_table.set active_row row);
  (match Hashtbl.find_opt by_id id with
  | None -> ()
  | Some row ->
      let t = get_row row in
      Lwd_table.set row { t with status = Active ts });
  t.active_id <- id

let set_resolved t id ts =
  map t (fun t ->
      if Int.equal t.Task.id id then { t with status = Resolved ts } else t)

let add (t : t) task =
  let set_selected = Option.is_none @@ Lwd_table.first t.table in
  (* Hmmmm *)
  let row =
    match Lwd_table.first t.table with
    | None -> Lwd_table.append ~set:task t.table
    | Some row -> Lwd_table.before ~set:task row
  in
  Hashtbl.add t.by_id task.id row;
  if set_selected then task.selected <- true;
  sort t

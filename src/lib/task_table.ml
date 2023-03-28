type t = {
  table : Task.t Lwd_table.t;
  mutable mode : Sort.t;
  mutable compare : Task.t -> Task.t -> int;
}

let set_sort_mode t mode =
  t.mode <- mode;
  t.compare <- Sort.compare mode

let create mode =
  { table = Lwd_table.make (); mode; compare = Sort.compare mode }

(* bubble sort. we need a sort that is linear when the table is already sorted. *)
let sort { table; compare; _ } =
  let rec bubble previous value =
    let p = Lwd_table.get previous |> Option.get in
    let v = Lwd_table.get value |> Option.get in
    if compare p v <= 0 then ()
    else (
      Lwd_table.set previous v;
      Lwd_table.set value p;
      match Lwd_table.prev previous with
      | None -> ()
      | Some ante -> bubble ante previous)
  in
  let rec loop previous = function
    | None -> ()
    | Some row ->
        bubble previous row;
        loop row (Lwd_table.next row)
  in
  match Lwd_table.first table with
  | None -> ()
  | Some first -> loop first (Lwd_table.next first)

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
        let t = Option.get (Lwd_table.get row) in
        let t' = f t in
        if t != t' then Lwd_table.set row t';
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

let update_loc { table; _ } id loc =
  map
    (fun t ->
      if Int.equal t.Task.id id then { t with loc = loc :: t.loc } else t)
    (Lwd_table.first table)

let update_logs { table; _ } id logs =
  map
    (fun t ->
      if Int.equal t.Task.id id then { t with logs = logs :: t.logs } else t)
    (Lwd_table.first table)

let update_name { table; _ } id name =
  map
    (fun t ->
      if Int.equal t.Task.id id then { t with name = name :: t.name } else t)
    (Lwd_table.first table)

let update_active { table; _ } ~domain ~id ts =
  map
    (fun t ->
      if Int.equal t.Task.id id && Int.equal t.Task.domain domain then
        { t with status = Active ts }
      else if Int.equal t.Task.domain domain then
        match t.status with
        | Active start ->
            Task.Busy.add t.busy (Int64.sub ts start);
            { t with status = Paused }
        | _ -> t
      else t)
    (Lwd_table.first table)

let set_resolved { table; _ } id ts =
  map
    (fun t ->
      if Int.equal t.Task.id id then { t with status = Resolved ts } else t)
    (Lwd_table.first table)

let add ({ table; compare; mode } : t) task =
  let set_selected = Option.is_none @@ Lwd_table.first table in
  (* Hmmmm *)
  (match Lwd_table.first table with
  | None -> Lwd_table.append' table task
  | Some row -> ignore (Lwd_table.before ~set:task row));
  if set_selected then task.selected <- true;
  sort { table; compare; mode }

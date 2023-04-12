type 'a tree = { children : 'a c; mutable parent : 'a c; mutable node : 'a }
and 'a c = 'a tree list ref

type t = {
  root : Task.t tree;
  pending : (int, Task.t) Hashtbl.t;
  by_id : (int, Task.t tree) Hashtbl.t;
  mutable active_id : int;
}

let make () =
  let by_id = Hashtbl.create 100 in
  let node =
    {
      (Task.create ~id:(-1) ~domain:0 ~parent_id:(-1) 0L Task) with
      name = [ "sleep" ];
      selected = ref true;
    }
  in
  let root = { node; parent = ref []; children = ref [] } in
  Hashtbl.add by_id (-1) root;
  { root; by_id; active_id = -1; pending = Hashtbl.create 100 }

let node p task = { children = ref []; node = task; parent = p }
let add t (task : Task.t) = Hashtbl.add t.pending task.id task

let update t id fn =
  match Hashtbl.find_opt t.by_id id with
  | None -> (
      match Hashtbl.find_opt t.pending id with
      | None -> Logs.warn (fun f -> f "Couldn't update fiber %d" id)
      | Some v -> Hashtbl.replace t.pending v.id (fn v))
  | Some v -> v.node <- fn v.node

let update_active t ~id ts =
  update t t.active_id (fun node ->
      match node.status with
      | Active start ->
          Task.Busy.add node.busy (Int64.sub ts start);
          { node with status = Paused ts }
      | _ -> node);
  update t id (fun node -> { node with status = Active ts });
  t.active_id <- id

let set_parent t ~child ~parent =
  Logs.debug (fun f -> f "set parent %d -> %d" child parent);
  match Hashtbl.find_opt t.by_id parent with
  | None -> ()
  | Some parent -> (
      match Hashtbl.find_opt t.pending child with
      | Some child_task ->
          Logs.debug (fun f -> f "new child %d" child);
          let node = node parent.children child_task in
          parent.children := node :: !(parent.children);
          Hashtbl.remove t.pending child;
          Hashtbl.add t.by_id child node
      | None -> (
          match Hashtbl.find_opt t.by_id child with
          | None -> ()
          | Some child ->
              child.parent :=
                List.filter
                  (fun v -> v.node.Task.id <> child.node.id)
                  !(child.parent);

              parent.children := child :: !(parent.children);
              child.parent <- parent.children))

let rec fold_node (t : 'a tree) fn acc =
  List.fold_left (fun a b -> fold_node b fn a) (fn acc t.node) !(t.children)

let fold t fn acc = fold_node t.root fn acc
let iter t fn = fold t (fun () v -> fn v) () |> ignore

let iter_mut t fn =
  let rec fold_loop t =
    t.node <- fn t.node;
    List.iter fold_loop !(t.children)
  in
  fold_loop t.root

let iter_with_prev t fn =
  fold t
    (fun prev v ->
      fn ~prev v;
      Some v)
    None
  |> ignore

type flatten_info = { last : bool; active : bool }

let merge_status st1 st2 =
  match (st1, st2) with
  | Task.Active a, _ -> Task.Active a
  | _, Task.Active b -> Active b
  | Paused a, Paused b when a > b -> Paused a
  | _, Paused b -> Paused b
  | Paused a, _ -> Paused a
  | Resolved a, Resolved b when a > b -> Resolved a
  | _, Resolved b -> Resolved b

let merge_stats ~(base : Task.t) (t : Task.t) =
  if !(t.selected) then (
    base.selected := true;
    t.selected := false);
  {
    base with
    busy = Task.Busy.merge base.busy t.busy;
    status = merge_status base.status t.status;
  }

let flatten t map =
  let rec map_loop ~depth t =
    let show_children =
      match (!(t.node.Task.display), Task.is_active t.node) with
      | Yes, _ -> true
      | Auto, true -> true
      | Toggle_requested, active ->
          t.node.Task.display := if active then No else Yes;
          not active
      | No, _ -> false
      | Auto, false -> false
    in
    let filtered = if show_children then false else !(t.children) <> [] in

    let node =
      if show_children then t.node
      else
        fold_node t
          (fun acc t ->
            match acc with
            | None -> Some t
            | Some base -> Some (merge_stats ~base t))
          None
        |> Option.get
    in

    let v = map ~depth ~filtered node in

    if filtered then Seq.return v
    else
      let children = !(t.children) |> List.to_seq in
      let next =
        Seq.append
          (children |> Seq.drop 1 |> Seq.map Option.some)
          (Seq.return None)
      in
      Seq.zip children next
      |> Seq.flat_map (fun (child, next) ->
             let depth =
               (match next with
               | None -> { last = true; active = false }
               | Some { node = { Task.status = Resolved _; _ } } ->
                   { last = false; active = false }
               | _ -> { last = false; active = true })
               :: depth
             in
             map_loop ~depth child)
      |> Seq.cons v
  in
  map_loop ~depth:[] t.root

let find_first t fn =
  let rec loop t =
    if fn t.node then Some t.node else List.find_map loop !(t.children)
  in
  loop t.root

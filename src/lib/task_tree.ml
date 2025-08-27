type 'a tree = { children : 'a c; mutable parent : 'a c; mutable node : 'a }
and 'a c = 'a tree list ref

type t = {
  root : Task.t tree;
  pending : (Task.Id.eio, Task.t) Hashtbl.t;
  by_id : (Task.Id.eio, Task.t tree) Hashtbl.t;
  mutable active_id : Task.Id.eio;
  mutable waiters : Task.t option Lwd.prim list;
}

let make () =
  let by_id = Hashtbl.create 100 in
  let node =
    {
      (Task.create ~id:(-1) ~domain:0 ~parent_id:(-1) (Timestamp.current ())
         (`Create (-1, `Fiber_in (-1))))
      with
      name = [ "sleep" ];
      selected = ref true;
    }
  in
  let root = { node; parent = ref []; children = ref [] } in
  let eio_id = Task.Id.eio node.id in
  Hashtbl.add by_id eio_id root;
  {
    root;
    by_id;
    active_id = eio_id;
    pending = Hashtbl.create 100;
    waiters = [];
  }

let node p task = { children = ref []; node = task; parent = p }
let invalidate t = List.iter Lwd.invalidate t.waiters

let add t (task : Task.t) =
  match task.kind with
  | `Create (_, (`Fiber_in _ | `Cc _)) -> (
      match Hashtbl.find_opt t.by_id (Task.Id.eio_of_int task.parent_id) with
      | None ->
          Logs.warn (fun f ->
              f "Couldn't find parent %d of %a" task.parent_id Task.Id.pp
                task.id)
      | Some p ->
          let node = node p.children task in
          Hashtbl.add t.by_id (Task.Id.eio task.id) node;
          p.children := node :: !(p.children);
          invalidate t)
  | _ ->
      Hashtbl.add t.pending (Task.Id.eio task.id) task;
      invalidate t

let update t id fn =
  match Hashtbl.find_opt t.by_id id with
  | None -> (
      match Hashtbl.find_opt t.pending id with
      | None ->
          Logs.warn (fun f -> f "Couldn't update fiber %a" Task.Id.pp_eio id)
      | Some v ->
          Hashtbl.replace t.pending id (fn v);
          invalidate t)
  | Some v ->
      v.node <- fn v.node;
      invalidate t

let update_active t ~id ts =
  update t t.active_id (fun node ->
      match node.status with
      | Active start ->
          let value = Int64.sub ts start in
          if value < 0L then
            Logs.err (fun f ->
                f "Invalid timestamp for %a (%Ld)" Task.Id.pp_eio t.active_id
                  value)
          else Task.Busy.add node.busy value;
          { node with status = Paused ts }
      | _ -> node);
  update t id (fun node -> { node with status = Active ts });
  t.active_id <- id;
  invalidate t

let is_cancellation_context task =
  match task.Task.kind with `Create (_, `Cc _) -> true | _ -> false

let set_parent t ~child ~parent ts =
  Logs.debug (fun f ->
      f "set parent %a -> %a" Task.Id.pp_eio child Task.Id.pp_eio parent);
  match Hashtbl.find_opt t.by_id parent with
  | None -> ()
  | Some parent -> (
      match Hashtbl.find_opt t.pending child with
      | Some child_task ->
          Logs.debug (fun f -> f "new child %a" Task.Id.pp_eio child);
          let node = node parent.children child_task in
          parent.children := node :: !(parent.children);
          Hashtbl.remove t.pending child;
          Hashtbl.add t.by_id child node;
          invalidate t
      | None -> (
          match Hashtbl.find_opt t.by_id child with
          | None -> ()
          | Some child when is_cancellation_context child.node ->
              (* don't move cancellation contexts around *) ()
          | Some child -> (
              let parent_already_has_child =
                List.find_opt
                  (fun c ->
                    Task.Id.eio c.node.Task.id = Task.Id.eio child.node.id)
                  !(parent.children)
              in
              match parent_already_has_child with
              | None ->
                  (* child goes in a sub context. We fork the task.
                   *)
                  let new_child =
                    {
                      child.node with
                      id = Task.Id.fork child.node.id;
                      busy = Task.Busy.make ();
                      name = child.node.name @ parent.node.name;
                      loc = parent.node.loc;
                      selected = ref false;
                      display = ref Task.Auto;
                    }
                  in
                  child.node <- { child.node with status = Paused ts };
                  let new_child_node = node parent.children new_child in
                  parent.children := new_child_node :: !(parent.children);
                  new_child_node.parent <- parent.children;
                  Hashtbl.replace t.by_id (Task.Id.eio new_child.id)
                    new_child_node;
                  invalidate t
              | Some parent_child ->
                  child.node <- { child.node with status = Resolved ts };
                  Hashtbl.replace t.by_id
                    (Task.Id.eio parent_child.node.Task.id)
                    parent_child;
                  invalidate t)))

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

type flatten_info = { last : bool; active : bool; cancellation_context : bool }

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
      let children =
        !(t.children)
        |> List.sort (fun a b -> Sort.compare Id a.node b.node)
        |> List.to_seq
      in
      let next =
        Seq.append
          (children |> Seq.drop 1 |> Seq.map Option.some)
          (Seq.return None)
      in
      let cancellation_context = is_cancellation_context t.node in
      Seq.zip children next
      |> Seq.flat_map (fun (child, next) ->
             let depth =
               (match next with
               | None -> { last = true; active = false; cancellation_context }
               | Some { node = { Task.status = Resolved _; _ }; _ } ->
                   { last = false; active = false; cancellation_context }
               | _ -> { last = false; active = true; cancellation_context })
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

let find_first_lwd t fn =
  Lwd.prim
    ~acquire:(fun prim ->
      t.waiters <- prim :: t.waiters;
      find_first t fn)
    ~release:(fun prim _ ->
      t.waiters <- List.filter (fun x -> x != prim) t.waiters)
  |> Lwd.get_prim

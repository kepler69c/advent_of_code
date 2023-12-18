let read_heat () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop ((String.to_seq s |> Seq.map (fun c -> int_of_char c - int_of_char '0') |> Array.of_seq) :: acc)
  | None -> List.rev acc in
  loop [] |> Array.of_list

let dims grid =
  let size_y = Array.length grid in
  if size_y > 0 then
    let size_x = Array.length grid.(0) in
    size_x, size_y
  else 0, 0

type direction = N | S | W | E | O

type node = {
  pos: int * int;
  dir: direction;
  steps: int
}

let shortest_path heat_map start objective =
  let dim_x, dim_y = dims heat_map in
  let inv = function | N -> S | S -> N | E -> W | W -> E | O -> O in
  let neighbors n =
    let x, y = n.pos in
    List.filter_map (fun (x, y, dir) ->
      let steps = if n.dir = dir then n.steps + 1 else 1 in
      if x >= 0 && x < dim_x && y >= 0 && y < dim_y && steps <= 10 && dir <> inv n.dir then
        Some { pos = (x, y); dir; steps }
      else None) (List.filter (fun (x, y, dir) ->
        n.steps >= 4 || dir = n.dir || n.dir = O) [(x - 1, y, W); (x + 1, y, E); (x, y - 1, N); (x, y + 1, S)])
  and all_nodes_seq = Seq.(ints 0 |> take dim_x |> map (fun x -> 
    Seq.(ints 0 |> take dim_y |> map (fun y -> 
      List.to_seq [N; S; W; E] |> Seq.map (fun dir -> 
        Seq.(ints 1 |> take 10 |> map (fun steps ->
            { pos = (x, y); dir; steps })))))) |> concat |> concat |> concat)
  and dist = Hashtbl.create 42
  and start_node = { pos = start; dir = O; steps = 0 } in
  Hashtbl.add_seq dist (Seq.map (fun n -> (n, max_int)) all_nodes_seq);
  Hashtbl.add dist start_node 0;
  let module NodeMap = Set.Make(struct type t=node 
    let compare n1 n2 =
      let comp = compare (Hashtbl.find dist n1) (Hashtbl.find dist n2) in
      if comp = 0 then compare n1 n2 else comp
  end) in
  let q = NodeMap.(of_seq all_nodes_seq |> add start_node) in
  let min_node q = NodeMap.min_elt q in
  let change_dist n d q =
    let dn = Hashtbl.find dist n in
    if d < dn then begin
      let b = NodeMap.mem n q in
      let q = if b then NodeMap.remove n q else q in (* DUMB THING *)
      Hashtbl.replace dist n d;
      if b then NodeMap.add n q else q
    end else q in
  let rec dijkstra q =
    if q <> NodeMap.empty then
      let u = min_node q in
      let q = NodeMap.remove u q in
      let du = Hashtbl.find dist u in
      let q = if du <> max_int then
        List.fold_left (fun q v -> 
          let alt = du + heat_map.(snd v.pos).(fst v.pos) in 
          change_dist v alt q) q
        (List.filter (fun v -> NodeMap.mem v q) (neighbors u)) else q in
      dijkstra q in
  dijkstra q;
  Hashtbl.to_seq dist |> Seq.filter (fun (n, _) -> n.pos = objective) |> Seq.fold_left (fun a (_, v) -> (min a v)) max_int

let () =
  let heat_map = read_heat () in
  let dim_x, dim_y = dims heat_map in
  let ret = shortest_path heat_map (0, 0) (dim_x - 1, dim_y - 1) in
  Printf.printf "%d\n" ret

type maze_tile = Empty | Wall | LSlope | RSlope | USlope | DSlope
type direction = L | R | U | D

let read_maze () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = 
    match try_read () with
    | Some s -> (String.to_seq s |> List.of_seq)::(loop acc)
    | None -> List.rev acc in
  let ret = loop [] in
  let ret = List.map (List.map (function
    | '.' -> Empty
    | '#' -> Wall
    | '>' -> RSlope
    | '<' -> LSlope
    | '^' -> USlope
    | 'v' -> DSlope
    | _ -> assert false)) ret in
  (List.map Array.of_list ret |> Array.of_list)

let dims grid =
  let size_y = Array.length grid in
  if size_y > 0 then
    let size_x = Array.length grid.(0) in
    size_x, size_y
  else 0, 0

let extract_graph maze start exit =
  let module NodeSet = Set.Make(struct 
    type t = int*int
    let compare = compare
  end) in
  let dim_x, dim_y = dims maze in
  let neighbors x y =
    if maze.(y).(x) = Wall then [] else
    List.filter (fun ((x, y), dir) ->
      x >= 0 && y >= 0 && x < dim_x && y < dim_y &&
      match maze.(y).(x), dir with
      | Empty, _
      | LSlope, _
      | RSlope, _
      | DSlope, _
      | USlope, _ -> true
      | _, _ -> false) [((x+1, y), R); ((x-1, y), L); ((x, y+1), D); ((x, y-1), U)] |>
    List.map fst in
  let all = Seq.ints 0 |> Seq.take dim_x |> Seq.map (fun x ->
    Seq.ints 0 |> Seq.take dim_y |> Seq.map (fun y -> (x, y))) |> Seq.concat in
  let nodes = [start] @ List.filter (fun (x, y) ->
    List.length (neighbors x y) >= 3) (List.of_seq all) @ [exit] in
  let successors = List.map (fun u ->
    let rec find_next_node x y visited len =
      if List.mem (x, y) nodes then ((x, y), len) else
      let n = List.filter (fun x -> not (NodeSet.mem x visited)) (neighbors x y) in
      assert (List.length n = 1);
      let x', y' = List.hd n in
      find_next_node x' y' (NodeSet.add (x, y) visited) (len + 1) in
    let x, y = u in
    let n = neighbors x y in
    List.map (fun (x, y) -> 
      find_next_node x y NodeSet.(empty |> add u) 1) n) nodes in
  Array.map (fun l ->
    let find_i list u =
      Seq.find_map (fun (i, v) -> if u = v then Some i else None) 
      (Seq.zip (Seq.ints 0) (List.to_seq list)) |> Option.get in
    List.map (fun (u, c) -> (find_i nodes u, c)) l) (Array.of_list successors)

let explore_paths graph =
  let module IS = Set.Make(Int) in
  let start = 0 in
  let exit = Array.length graph - 1 in
  let rec loop path visited =
    let u = List.hd path in
    if u = exit then [path] else
    let new_nodes = 
      List.filter (fun (x, _) -> not (IS.mem x visited)) (graph.(u)) |> List.map fst in
    if new_nodes = [] then [] else
    let new_paths = List.map (fun x -> x :: path) new_nodes in
    let visited = IS.add u visited in
    List.fold_left (fun acc path -> acc @ (loop path visited)) [] new_paths in
  loop [start] IS.empty |> List.map List.rev

let () =
  let maze = read_maze () in
  let dim_x, dim_y = dims maze in
  let start = (1, 0) in
  let exit = (dim_x - 2, dim_y - 1) in
  let graph = extract_graph maze start exit in
  let paths = explore_paths graph in
  let get_weight u v =
    List.find_map (fun (v', c) ->
      if v = v' then Some c else None) graph.(u) |> Option.get in
  let ret = List.fold_left max 0 (List.map (fun l ->
    let couples = 
      Seq.zip (List.to_seq l) (List.(tl l |> to_seq)) |> List.of_seq in
    List.fold_left (fun acc (u, v) -> acc + get_weight u v) 0 couples) paths) in
  Printf.printf "%d\n" ret

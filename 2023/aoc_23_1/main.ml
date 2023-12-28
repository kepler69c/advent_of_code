type maze_time = Empty | Wall | LSlope | RSlope | USlope | DSlope

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

let neighbors maze x y =
  let dim_x, dim_y = dims maze in
  List.filter (fun ((x, y), dir) ->
    x >= 0 && y >= 0 && x < dim_x && y < dim_y &&
    match maze.(y).(x), dir with
    | Empty, _
    | LSlope, L
    | RSlope, R
    | DSlope, D
    | USlope, U -> true
    | _, _ -> false) [((x+1, y), R); ((x-1, y), L); ((x, y+1), D); ((x, y-1), U)] |>
  List.map fst

let explore_paths maze start exit =
  let module NodeSet = Set.Make(struct 
    type t = int*int
    let compare = compare
  end) in
  let rec loop path visited =
    let x, y = List.hd path in
    if (x, y) = exit then [path] else
    let new_nodes = List.filter (fun x -> not (NodeSet.mem x visited)) (neighbors maze x y) in
    if new_nodes = [] then [] else
    let new_paths = List.map (fun x -> x :: path) new_nodes in
    let visited = NodeSet.add (x, y) visited in
    List.fold_left (fun acc path -> acc @ (loop path visited)) [] new_paths in
  loop [start] NodeSet.empty

let () =
  let maze = read_maze () in
  let dim_x, dim_y = dims maze in
  let start = (1, 0) in
  let exit = (dim_x - 2, dim_y - 1) in
  let paths = explore_paths maze start exit in
  let ret = List.fold_left max 0 (List.map List.length paths) - 1 in
  Printf.printf "%d\n" ret

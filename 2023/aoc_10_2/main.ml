type connect =
  | Ground
  | Start
  | NS
  | EW
  | NE
  | NW
  | SW
  | SE

type dir =
  | E | W | N | S

let char_to_connect c =
  match c with
  | '.' -> Ground
  | 'S' -> Start
  | '|' -> NS
  | '-' -> EW
  | 'L' -> NE
  | 'J' -> NW
  | '7' -> SW
  | 'F' -> SE
  | _ -> assert false

let line_to_maze s =
  Seq.map char_to_connect (String.to_seq s) |> Array.of_seq

let read_maze () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop ((line_to_maze s) :: acc)
  | None -> List.rev acc in
  loop [] |> Array.of_list

let traverse_next x y d v_maze =
  match fst v_maze.(y).(x), d with
  | NS, S | SW, E | SE, W | Start, S -> (x, y + 1, S)
  | NS, N | NE, W | NW, E | Start, N -> (x, y - 1, N)
  | EW, E | NE, S | SE, N | Start, E -> (x + 1, y, E)
  | EW, W | NW, S | SW, N | Start, W -> (x - 1, y, W)
  | _, _ -> assert false

let maze_dims maze =
  let size_y = Array.length maze in
  if size_y > 0 then 
    let size_x = Array.length maze.(0) in
    size_x, size_y
  else 0, 0

let maze_access x y maze =
  let size_x, size_y = maze_dims maze in
  if x >= 0 && x < size_x && y >= 0 && y < size_y then
    Some (x, y)
  else None 

let get_maze x y maze =
  let size_x, size_y = maze_dims maze in
  if x >= 0 && x < size_x && y >= 0 && y < size_y then
    Some (maze.(y).(x))
  else None 

let valid_starts x y maze =
  List.filter_map (fun x -> x) [(match get_maze (x + 1) y maze with
  | Some EW | Some NW | Some SW -> Some E
  | _ -> None);
  (match get_maze (x - 1) y maze with
  | Some EW | Some NE | Some SE -> Some W
  | _ -> None);
  (match get_maze x (y + 1) maze with
  | Some NS | Some NW | Some NE -> Some S
  | _ -> None);
  (match get_maze x (y - 1) maze with
  | Some NS | Some SW | Some SE -> Some N
  | _ -> None)]

let inc_piped prev tile =
  match prev, tile with
  | _,  NS
  | SE, NW
  | NE, SW -> true
  | _ -> false

let () =
  let maze = read_maze () in
  let x, y = 
    Seq.find (fun ((x, y), c) ->
      c = Start) (Seq.concat (Seq.mapi (fun y a ->
        Seq.mapi (fun x c ->
          ((x, y), c)) (Array.to_seq a)) (Array.to_seq maze))) |> Option.get |> fst in
  let dir1, dir2 = match valid_starts x y maze with
  | [a; b] -> a, b
  | _ -> assert false in
  let v_maze = Array.map (fun a -> Array.map (fun c -> (c, false)) a) maze in
  v_maze.(y).(x) <- (fst v_maze.(y).(x), true);
  let rec mark_pipe x1 y1 dir1 x2 y2 dir2 v_maze =
    let x1, y1, dir1 = traverse_next x1 y1 dir1 v_maze in
    let x2, y2, dir2 = traverse_next x2 y2 dir2 v_maze in
    v_maze.(y1).(x1) <- (fst v_maze.(y1).(x1), true);
    v_maze.(y2).(x2) <- (fst v_maze.(y2).(x2), true);
    if x1 = x2 && y1 = y2 then ()
    else mark_pipe x1 y1 dir1 x2 y2 dir2 v_maze in
  mark_pipe x y dir1 x y dir2 v_maze;
  let maze = Array.map (fun a -> Array.map (fun (c, b) -> if b then c else Ground) a) v_maze in
  maze.(y).(x) <- (match dir1, dir2 with
  | W, E | E, W -> EW
  | N, S | S, N -> NS
  | N, W | W, N -> NW
  | S, W | W, S -> SW
  | N, E | E, N -> NE
  | S, E | E, S -> SE
  | _, _ -> assert false);
  let cnts = Array.map (fun a -> Array.fold_left (fun (cnt, (prev, piped)) c ->
    ((if (c = Ground) && ((piped mod 2) = 1) then cnt + 1 else cnt), 
    ((if c = EW then prev else c), (if inc_piped prev c then piped + 1 else piped)))) (0, (Ground, 0)) a |> fst) maze in
  let ret = Array.fold_left ( + ) 0 cnts in
  Printf.printf "%d\n" ret

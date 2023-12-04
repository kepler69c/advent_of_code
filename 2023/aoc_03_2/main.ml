let read_lines =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop []

let get_map map x y =
  let size_y = Array.length map in
  let size_x = if size_y = 0 then 0 else Array.length map.(0) in
  if x >= 0 && x < size_x && y >= 0 && y < size_y then
    Some map.(y).(x)
  else
    None

let neighbor_map = [(-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1)]
let get_neighbors map x y =
  List.map (fun (x_off, y_off) -> get_map map (x + x_off) (y + y_off)) neighbor_map

let is_num c = c >= '0' && c <= '9'
let char_to_int c = (Char.code c) - (Char.code '0')

let get_num map x y =
  let size_y = Array.length map in
  let size_x = if size_y = 0 then 0 else Array.length map.(0) in
  let rec goright map x y =
    if x < size_x && is_num map.(y).(x) then
      goright map (x + 1) y
    else (x - 1, y) in
  let x, y = goright map x y in
  let rec goleft map x y =
    if x >= 0 && is_num map.(y).(x) then
      (char_to_int map.(y).(x)) + 10 * (goleft map (x - 1) y)
    else 0 in
  goleft map x y
let count_num_row row map x y =
  match row with
  | [true ; false; true ] -> (2, (get_num map (x - 1) y) * (get_num map (x + 1) y))
  | [true ; false; false] -> (1, get_num map (x - 1) y)
  | [true ; true ; false]
  | [false; true ; false] -> (1, get_num map x y)
  | [false; false; true ]
  | [false; true ; true ]
  | [true ; true ; true ] -> (1, get_num map (x + 1) y)
  | _ -> (0, 1)
let filter_num = List.map (fun e -> match e with | Some c -> is_num c | None -> false)
let analyze_neighbors map x y =
  let neighbors = get_neighbors map x y in
  let top = filter_num [List.nth neighbors 0; List.nth neighbors 1; List.nth neighbors 2] in
  let mid = filter_num [List.nth neighbors 3; None                ; List.nth neighbors 4] in
  let bot = filter_num [List.nth neighbors 5; List.nth neighbors 6; List.nth neighbors 7] in
  let top_c, top_g = count_num_row top map x (y - 1) in
  let mid_c, mid_g = count_num_row mid map x y in
  let bot_c, bot_g = count_num_row bot map x (y + 1) in
  if top_c + mid_c + bot_c = 2 then 
    top_g * mid_g * bot_g else 0

let () =
  let lines = read_lines in
  let map = List.map (fun s -> Array.of_seq (String.to_seq s)) lines |> Array.of_list in
  let ret = Array.fold_left (fun (acc_y, y) a -> 
    let acc_x = Array.fold_left (fun (acc_x, x) e ->
      (acc_x + (if e = '*' then analyze_neighbors map x y else 0), x + 1)) (0, 0) a |> fst in
      (acc_x + acc_y, y + 1)) (0, 0) map |> fst in
  Printf.printf "%d\n" ret

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
    Some (map.(y)).(x)
  else
    None

let get_neighbors map x y =
  let access = [(-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1)] in
  List.map (fun (x_off, y_off) -> get_map map (x + x_off) (y + y_off)) access

let is_num c = c >= '0' && c <= '9'
let char_to_int c = (Char.code c) - (Char.code '0')

let is_symbol c = c <> '.' && not (is_num c)

let neighbor_symbol map x y =
  List.exists is_symbol (List.filter_map (fun p -> p) (get_neighbors map x y))

let () =
  let lines = read_lines in
  let map = List.map (fun s -> Array.of_seq (String.to_seq s)) lines |> Array.of_list in
  let ret = Array.fold_left (fun (acc_y, y) a ->
    match (Array.fold_left (fun (acc_x, x, num, symb) c ->
      match num, is_num c, symb with
      | Some n, true, b -> (acc_x, x + 1, Some (10 * n + (char_to_int c)), b || (neighbor_symbol map x y))
      | None, true, _ -> (acc_x, x + 1, Some (char_to_int c), (neighbor_symbol map x y))
      | Some n, false, b -> (acc_x + (if b then n else 0), x + 1, None, false)
      | _, _, _ -> (acc_x, x + 1, None, false)) (0, 0, None, false) a) with
    | acc_x, _, Some n, true -> (acc_x + acc_y + n, y + 1)
    | acc_x, _, _, _ -> (acc_x + acc_y, y + 1)) (0, 0) map |> fst in
  Printf.printf "%d\n" ret

let line_to_galaxy s =
  Seq.map (function | '#' -> true | '.' -> false | _ -> assert false) (String.to_seq s) |> Array.of_seq

let read_galaxy () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop ((line_to_galaxy s) :: acc)
  | None -> List.rev acc in
  loop [] |> Array.of_list

let expand_vertical galaxy = 
  Array.fold_left (fun l a ->
    (if (Array.for_all (fun b -> not b) a) then [a; a] else [a]) @ l) [] galaxy |> Array.of_list

let expand galaxy =
  let ex_horz = expand_vertical galaxy in
  let transpose = Array.map Array.to_seq ex_horz |> Array.to_seq |> Seq.transpose |> Array.of_seq |> Array.map Array.of_seq in
  expand_vertical transpose

let rec get_couples l =
  match l with
  | x::l' -> (List.map (fun y -> (x, y)) l') @ (get_couples l')
  | [] -> []

let dist n1 n2 =
  let x1, y1 = n1 in let x2, y2 = n2 in
  Int.abs (x2 - x1) + Int.abs (y2 - y1)

let get_stars galaxy =
  Array.to_seqi galaxy |>
  Seq.map (fun (y, s) -> Array.to_seqi s |> 
    Seq.map (fun (x, c) -> (x, y, c))) |>
  Seq.concat |>
  Seq.filter_map (fun (x, y, c) -> if c then Some (x, y) else None) |>
  List.of_seq

let () =
  let galaxy = read_galaxy () in
  let expanded = expand galaxy in
  let distances = List.map (fun (n1, n2) -> dist n1 n2) (get_couples (get_stars expanded)) in
  let ret = List.fold_left ( + ) 0 distances in
  Printf.printf "%d\n" ret

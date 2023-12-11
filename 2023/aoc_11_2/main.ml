type galaxy_unit = | Normal | Million | Star

let line_to_galaxy s =
  Seq.map (function | '#' -> Star | '.' -> Normal | _ -> assert false) (String.to_seq s) |> Array.of_seq

let read_galaxy () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop ((line_to_galaxy s) :: acc)
  | None -> List.rev acc in
  loop [] |> Array.of_list

let expand_vertical galaxy = 
  Array.fold_left (fun l a ->
    (if (Array.for_all (fun b -> b <> Star) a) then 
      [Array.make (Array.length a) Million] else [a]) @ l) [] galaxy |> Array.of_list

let expand galaxy =
  let ex_vert = expand_vertical galaxy in
  let transpose = Array.map Array.to_seq ex_vert |> Array.to_seq |> Seq.transpose |> Array.of_seq |> Array.map Array.of_seq in
  expand_vertical transpose

let rec get_couples l =
  match l with
  | x::l' -> (List.map (fun y -> (x, y)) l') @ (get_couples l')
  | [] -> []

let dist n1 n2 =
  let x1, y1 = n1 in let x2, y2 = n2 in
  Int.abs (x2 - x1) + Int.abs (y2 - y1)

let get_stars galaxy =
  Array.fold_left_map (fun acc_y a -> 
    let a' = Array.fold_left_map (fun acc_x c ->
    match c with
    | Million -> (acc_x + 1_000_000, (acc_x, c))
    | _ -> (acc_x + 1, (acc_x, c))) 0 a |> snd in
    if Array.for_all (fun c -> c = Million) a then
      (acc_y + 1_000_000, Array.map (fun c -> (0, acc_y, c)) a)
    else
      (acc_y + 1, Array.map (fun (x, c) -> (x, acc_y, c)) a')) 0 galaxy |> snd |>
  Array.to_seq |> Seq.map (Array.to_seq) |> Seq.concat |> 
  Seq.filter_map (fun (x, y, c) -> if c = Star then Some (x, y) else None) |> List.of_seq

let () =
  let galaxy = read_galaxy () in
  let expanded = expand galaxy in
  let distances = List.map (fun (n1, n2) -> dist n1 n2) (get_couples (get_stars expanded)) in
  let ret = List.fold_left ( + ) 0 distances in
  Printf.printf "%d\n" ret

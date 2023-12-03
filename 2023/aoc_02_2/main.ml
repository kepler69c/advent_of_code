let read_lines =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop []

let read_trun line =
  let l = String.split_on_char ',' line in
  List.fold_left (fun (r, g, b) e ->
    let n, c = Scanf.sscanf e " %d %s" (fun n c -> (n, c)) in
    match c with
    | "red"   -> (r+n, g, b)
    | "green" -> (r, g+n, b)
    | "blue"  -> (r, g, b+n)
    | _ -> (r, g, b)) (0, 0, 0) l

let read_games line =
  let games = List.nth (String.split_on_char ':' line) 1 in
  let games = String.split_on_char ';' games in
  List.map read_trun games

let () =
  let lines = read_lines in
  let games = List.map read_games lines in
  let ret = List.fold_left (fun acc game ->
    let r, g, b = List.fold_left (fun (r_max, g_max, b_max) (r, g, b) ->
      (Int.max r_max r, Int.max g_max g, Int.max b_max b)) (0, 0, 0) game in
    acc + r * g * b) 0 games in
  Printf.printf "%d\n" ret

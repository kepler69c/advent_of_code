let read_lines =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop []

let read_numbers nums =
  let try_read s =
    try Some (Scanf.sscanf s " %d %n" (fun p n -> (p, n))) with End_of_file -> None in
  let rec loop acc s = match try_read s with
  | Some (p, n) -> loop (p :: acc) (String.sub s n (String.length s - n))
  | None -> List.rev acc in
  loop [] nums

module IS = Set.Make(Int)

let collect_cards line =
  let game = List.nth (String.split_on_char ':' line) 1 in
  let game = String.split_on_char '|' game in
  let win, cards = String.trim (List.nth game 0), String.trim (List.nth game 1) in
  let win = read_numbers win in
  let cards = read_numbers cards in
  let win = IS.of_list win in
  let cards = IS.of_list cards in
  (Int.shift_left 1 (IS.cardinal (IS.inter win cards))) / 2

let () =
  let lines = read_lines in
  let ret = List.fold_left (fun a s -> a + s) 0 (List.map collect_cards lines) in
  Printf.printf "%d\n" ret

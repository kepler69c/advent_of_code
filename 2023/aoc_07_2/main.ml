let card_value c =
  match c with
  | 'J' -> 0
  | '2' -> 1
  | '3' -> 2
  | '4' -> 3
  | '5' -> 4
  | '6' -> 5
  | '7' -> 6
  | '8' -> 7
  | '9' -> 8
  | 'T' -> 9
  | 'Q' -> 10
  | 'K' -> 11
  | 'A' -> 12
  | _ -> assert false

let read_lines =
  let try_read () =
    try Some (Scanf.scanf "%c%c%c%c%c %d\n" (fun c1 c2 c3 c4 c5 b ->
      (Array.map card_value [|c1; c2; c3; c4; c5|], b))) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop []

let determine_type hand =
  let map = Hashtbl.create 42 in
  Array.iter (fun c ->
    match Hashtbl.find_opt map c with
    | Some n -> Hashtbl.replace map c (n + 1)
    | None -> Hashtbl.add map c 1) hand;
  let vals = Hashtbl.to_seq_values map in
  let hand_type = if Seq.exists (Int.equal 5) vals then 6
  else if Seq.exists (Int.equal 4) vals then 5
  else if Seq.exists (Int.equal 3) vals && Seq.exists (Int.equal 2) vals then 4
  else if Seq.exists (Int.equal 3) vals then 3
  else if (Seq.fold_left (fun acc v -> if v = 2 then acc + 1 else acc) 0 vals) = 2 then 2
  else if Seq.exists (Int.equal 2) vals then 1
  else 0 in
  match (match Hashtbl.find_opt map 0 with | None -> 0 | Some n -> n), hand_type with
  | 0, v -> v
  | 1, 0 -> 1
  | 1, 1 -> 3
  | 1, 2 -> 4
  | 1, 3 -> 5
  | 1, 5 -> 6
  | 2, 1 -> 3
  | 2, 2 -> 5
  | 2, 4 -> 6
  | 3, 3 -> 5
  | 3, 4 -> 6
  | 4, 5 -> 6
  | 5, 6 -> 6
  | _, _ -> assert false

let () =
  let hands = read_lines in
  let hands = List.map (fun (cards, bid) -> (determine_type cards, cards, bid)) hands in
  let hands = List.sort compare hands in
  let ret = List.fold_left (fun (acc, i) (_, _, bid) -> (acc + bid * i, i + 1)) (0, 1) hands |> fst in
  Printf.printf "%d\n" ret

let card_value c =
  match c with
  | '2' -> 0
  | '3' -> 1
  | '4' -> 2
  | '5' -> 3
  | '6' -> 4
  | '7' -> 5
  | '8' -> 6
  | '9' -> 7
  | 'T' -> 8
  | 'J' -> 9
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
  if Seq.exists (Int.equal 5) vals then 6
  else if Seq.exists (Int.equal 4) vals then 5
  else if Seq.exists (Int.equal 3) vals && Seq.exists (Int.equal 2) vals then 4
  else if Seq.exists (Int.equal 3) vals then 3
  else if (Seq.fold_left (fun acc v -> if v = 2 then acc + 1 else acc) 0 vals) = 2 then 2
  else if Seq.exists (Int.equal 2) vals then 1
  else 0


let () =
  let hands = read_lines in
  let hands = List.map (fun (cards, bid) -> (determine_type cards, cards, bid)) hands in
  let hands = List.sort compare hands in
  let ret = List.fold_left (fun (acc, i) (_, _, bid) -> (acc + bid * i, i + 1)) (0, 1) hands |> fst in
  Printf.printf "%d\n" ret

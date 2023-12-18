type direction = L | R | U | D

let read_plan () =
  let try_read () =
    try Some (Scanf.scanf "%c %d (#%c%c%c%c%c%c)\n" 
    (fun _ _ c1 c2 c3 c4 c5 d -> (c1, c2, c3, c4, c5, d))) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some (c1, c2, c3, c4, c5, d) ->
      let n = Scanf.sscanf (String.of_seq (List.to_seq [c1; c2; c3; c4; c5])) "%x" Fun.id in
      loop (((match d with | '0' -> R | '1' -> D | '2' -> L | '3' -> U | _ -> assert false), n)::acc)
  | None -> List.rev acc in
  loop []

let () =
  let plan = read_plan () in
  let points = List.fold_left (fun (l, (x, y)) (dir, n) ->
    let x, y = match dir with
    | L -> (x - n, y)
    | R -> (x + n, y)
    | U -> (x, y - n)
    | D -> (x, y + n) in
    ((x, y)::l, (x, y))) ([], (0, 0)) plan |> fst in
  let ret = 1 + List.fold_left (fun a ((xi, yi), (xj, _)) ->
    a + yi * (xi - xj)) (List.fold_left (+) 0 (List.map snd plan) / 2)
  (List.combine points (List.(rev points |> hd)::List.(rev points |> tl |> rev))) in
  Printf.printf "%d\n" ret

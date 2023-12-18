type direction = L | R | U | D

let read_plan () =
  let try_read () =
    try Some (Scanf.scanf "%c %d (#%x)\n" (fun dir n color -> (dir, n, color))) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some (dir, n, color) ->
      loop (((match dir with | 'L' -> L | 'R' -> R | 'U' -> U | 'D' -> D | _ -> assert false), n, color)::acc)
  | None -> List.rev acc in
  loop []

let () =
  let plan = read_plan () in
  let (min_x, max_x, min_y, max_y) = List.fold_left (fun ((min_x, max_x, min_y, max_y), (x, y)) (dir, n, _) ->
    let x, y = match dir with
    | L -> (x - n, y)
    | R -> (x + n, y)
    | U -> (x, y - n)
    | D -> (x, y + n) in
    ((min x min_x, max x max_x, min y min_y, max y max_y), (x, y))) ((0, 0, 0, 0), (0, 0)) plan |> fst in
  let dim_x, dim_y = (max_x - min_x + 3, max_y - min_y + 3) in
  let terrain = Array.make_matrix dim_y dim_x true in
  List.fold_left (fun (x, y) (dir, n,  _) ->
    match dir with
    | L -> Seq.iter (fun x -> terrain.(y).(x) <- false) (Seq.(ints 1 |> take n |> map ((-) x))); (x - n, y)
    | R -> Seq.iter (fun x -> terrain.(y).(x) <- false) (Seq.(ints 1 |> take n |> map ((+) x))); (x + n, y)
    | U -> Seq.iter (fun y -> terrain.(y).(x) <- false) (Seq.(ints 1 |> take n |> map ((-) y))); (x, y - n)
    | D -> Seq.iter (fun y -> terrain.(y).(x) <- false) (Seq.(ints 1 |> take n |> map ((+) y))); (x, y + n))
  (max 0 (-min_x) + 1, max 0 (-min_y) + 1) plan |> fun _ -> ();
  let rec dfs x y =
    if x >= 0 && x < dim_x && y >= 0 && y < dim_y && terrain.(y).(x) then
      (terrain.(y).(x) <- false;
      List.iter (fun (x, y) -> dfs x y) [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]) in
  dfs 0 0;
  List.fold_left (fun (x, y) (dir, n,  _) ->
    match dir with
    | L -> Seq.iter (fun x -> terrain.(y).(x) <- true) (Seq.(ints 1 |> take n |> map ((-) x))); (x - n, y)
    | R -> Seq.iter (fun x -> terrain.(y).(x) <- true) (Seq.(ints 1 |> take n |> map ((+) x))); (x + n, y)
    | U -> Seq.iter (fun y -> terrain.(y).(x) <- true) (Seq.(ints 1 |> take n |> map ((-) y))); (x, y - n)
    | D -> Seq.iter (fun y -> terrain.(y).(x) <- true) (Seq.(ints 1 |> take n |> map ((+) y))); (x, y + n))
  (max 0 (-min_x) + 1, max 0 (-min_y) + 1) plan |> fun _ -> ();
  Array.iter (fun a -> Array.iter (fun b -> Printf.printf (if b then "#" else ".")) a; Printf.printf "\n") terrain;
  let ret = Array.fold_left (fun acc a -> acc + Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 a) 0 terrain in
  Printf.printf "%d\n" ret

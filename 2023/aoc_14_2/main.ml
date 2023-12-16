let read_dish () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop ((String.to_seq s |> List.of_seq) :: acc)
  | None -> List.rev acc in
  loop []

let dims grid =
  let size_y = List.length grid in
  if size_y > 0 then
    let size_x = List.(hd grid |> length) in
    size_x, size_y
  else 0, 0

let rotate_dish dish =
  let dish' = Array.(of_list dish |> map Array.of_list) in
  let dish'' = Array.(of_list dish |> map Array.of_list) in
  let n = Array.length dish' in
  Array.iteri (fun y a -> Array.iteri (fun x c ->
    dish''.(x).(n - y - 1) <- dish'.(y).(x)) a) dish';
  Array.(map Array.to_list dish'' |> to_list)

let spin_billion dish =
  let dim_x, dim_y = dims dish in
  let apply_gravity_once dish =
    let dish = List.map Array.of_list dish |> Array.of_list in
    Seq.iter (fun y -> Seq.iter (fun x ->
      match dish.(y).(x), dish.(y - 1).(x) with
      | 'O', '.' -> dish.(y).(x) <- '.'; dish.(y - 1).(x) <- 'O';
      | _, _ -> ())
    (Seq.(ints 0 |> take dim_x))) (Seq.(ints 1 |> take (dim_y - 1)));
    Array.(map Array.to_list dish |> to_list) in
  let rec apply_gravity_fixpoint dish =
    let dish' = dish in
    let dish = apply_gravity_once dish in
    if dish <> dish' then apply_gravity_fixpoint dish
    else dish in  
  let spin dish =
    let dish' = apply_gravity_fixpoint dish in (* N *)
    let dish' = rotate_dish dish' in
    let dish' = apply_gravity_fixpoint dish' in (* W *)
    let dish' = rotate_dish dish' in
    let dish' = apply_gravity_fixpoint dish' in (* S *)
    let dish' = rotate_dish dish' in
    let dish' = apply_gravity_fixpoint dish' in (* E *)
    let dish' = rotate_dish dish' in
(*
    List.iter (fun l -> List.iter (Printf.printf "%c") l; Printf.printf "\n") dish';
    Printf.printf "\n";
*)
    dish' in
  let rec spin_loop dish saves =
    let dish' = spin dish in
    match List.find_opt (fun (i, dish) -> 
      dish = dish') (List.combine (Seq.(ints 0 |> take (List.length saves) |> List.of_seq)) (List.rev saves)) with
    | Some (i, dish) -> (i, dish, List.rev saves)
    | None -> spin_loop dish' (dish'::saves) in
  let n_loop, dish, spins = spin_loop dish [dish] in
  let loop = List.to_seq spins |> Seq.drop n_loop |> Array.of_seq in
  loop.((1_000_000_000 - n_loop) mod (Array.length loop))

let () =
  let dish = read_dish () in
  let dish = spin_billion dish in
  let ret = List.fold_left (fun (acc, i) l ->
    let cnt = List.fold_left (fun acc c ->
      match c with
      | 'O' -> acc + 1
      | _ -> acc) 0 l in
    (cnt * i + acc, i - 1)) (0, List.length dish) dish |> fst in
  Printf.printf "%d\n" ret

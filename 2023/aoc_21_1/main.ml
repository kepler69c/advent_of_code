let list_enumerate l =
  Seq.zip (Seq.ints 0) (List.to_seq l) |> List.of_seq

let read_plan () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = 
    match try_read () with
    | Some s -> (String.to_seq s |> List.of_seq)::(loop acc)
    | None -> List.rev acc in
  let ret = loop [] in
  let start = List.find_map (fun (y, l) ->
    List.find_map (fun (x, c) ->
      if c = 'S' then Some (x, y) else None)
    (list_enumerate l)) (list_enumerate ret) |> Option.get in
  let ret = List.map (List.map ((=) '#')) ret in
  (List.map Array.of_list ret |> Array.of_list), start


let dims grid =
  let size_y = Array.length grid in
  if size_y > 0 then
    let size_x = Array.length grid.(0) in
    size_x, size_y
  else 0, 0

let neighbors plan x y =
  let dim_x, dim_y = dims plan in
  List.filter (fun (x, y) -> 
    x >= 0 && y >= 0 && x < dim_x && y < dim_y && not (plan.(y).(x))) 
  (List.map (fun (ox, oy) -> 
    (ox + x, oy + y)) [(-1, 0); (1, 0); (0, -1); (0, 1)])

let () =
  let plan, start = read_plan () in
  let steps = Seq.fold_left (fun acc _ ->
    List.fold_left (fun acc (x, y) ->
      (neighbors plan x y) :: acc) [] acc |> 
    List.concat |> 
    List.sort_uniq compare) [start] (Seq.repeat () |> Seq.take 64) in
  let ret = List.length steps in
  Printf.printf "%d\n" ret

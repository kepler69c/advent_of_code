let line_to_lava s =
  Seq.map (function | '#' -> true | '.' -> false | _ -> assert false)
  (String.to_seq s) |> List.of_seq

let read_lava () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc cur_map = match try_read () with
  | Some "" -> loop ((List.rev cur_map)::acc) []
  | Some s -> loop acc ((line_to_lava s) :: cur_map)
  | None -> List.rev ((List.rev cur_map)::acc) in
  loop [] []

let get_mirror_index map = 
  List.fold_left (fun (i, acc) (line1, line2) ->
    (i+1, if line1 = line2 then i::acc else acc)) (1, [])
  (List.combine (List.tl map) (List.rev map |> List.tl |> List.rev)) |> snd

let lava_dims lava =
  let size_y = List.length lava in
  if size_y > 0 then
    let size_x = List.length (List.hd lava) in
    size_x, size_y
  else 0, 0

let filter_mirror_index indexes lava =
  let len = List.length lava in
  List.filter (fun index ->
    let lava, index = 
      if index > (len / 2) then 
        List.rev lava, (len - index) 
      else lava, index in
    let trunc_lava =
      Array.sub (Array.of_list lava) 0 (2 * index) |> Array.to_list in
    trunc_lava = (List.rev trunc_lava)) indexes

let get_score lava =
  let transpose =
    List.to_seq lava |>
    Seq.map List.to_seq |> 
    Seq.transpose |> 
    List.of_seq |> 
    List.map List.of_seq in
  let horz_may = get_mirror_index lava
  and vert_may = get_mirror_index transpose in
  let horz_mirrors = filter_mirror_index horz_may lava
  and vert_mirrors = filter_mirror_index vert_may transpose in
  100 * (List.fold_left ( + ) 0 horz_mirrors) + (List.fold_left ( + ) 0 vert_mirrors)

let () =
  let lava = read_lava () in
  let ret = List.fold_left (fun a lava -> a + get_score lava) 0 lava in
  Printf.printf "%d\n" ret

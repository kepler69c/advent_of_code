let read_lines =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop []

let is_num c = c >= '0' && c <= '9'

let get_mapping x map =
  match List.fold_left (fun a (dst, src, r) ->
    match a with
    | None -> if x >= src && x < (src + r) then 
      Some ((dst - src) + x)
    else None
    | a -> a) None map with
  | Some n -> n
  | None -> x

let () =
  let lines = read_lines in
  let seeds = List.nth lines 0 in
  let seeds = String.trim (List.nth (String.split_on_char ':' seeds) 1) in
  let seeds = String.split_on_char ' ' seeds in
  let seeds = List.map (fun s -> Scanf.sscanf s "%d" (fun p -> p)) seeds in
  let maps = match lines with | _::_::_::l -> l | _ -> [] in
  let maps = List.fold_left (fun map_acc l ->
    if l = "" then
      []::map_acc
    else if is_num l.[0] then
      let nums = Scanf.sscanf l "%d %d %d" (fun a b c -> (a, b, c)) in
      match map_acc with
      | x::l -> ((nums::x)::l)
      | [] -> map_acc
    else map_acc) [[]] maps |> List.rev in
  let ret_l = List.map (fun s ->
    List.fold_left (fun acc map ->
      get_mapping acc map) s maps) seeds in
  let ret = List.fold_left Int.min max_int ret_l in
  Printf.printf "%d\n" ret

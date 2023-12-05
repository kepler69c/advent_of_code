let read_lines =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop []

let is_num c = c >= '0' && c <= '9'

let get_range_mapping l_r map =
  let l_r = List.map (fun (x, r) -> (x, r, false)) l_r in
  let l_r = List.fold_left (fun acc_m (dst, src, r) ->
    List.fold_left (fun acc_r (ax, ar, m) ->
      if m then (ax, ar, m)::acc_r else
      let b_in = ax >= src && ax < (src + r)
      and e_in = (ax + ar) >= src && (ax + ar) < (src + r) in
      match b_in, e_in with
      | false, false ->
          if ax < src && (ax + ar) > (src + r) then
            (ax, ar - src + ax - r, false)::(dst, r, true)::(2*ax + ar - src, ar - src + ax, false)::acc_r
          else (ax, ar, false)::acc_r
      | false, true  -> (ax, src - ax, false)::(dst, ar - (src - ax), true)::acc_r
      | true,  false -> (dst - src + ax, src + r - ax, true)::(src + r, ar - (src + r - ax), false)::acc_r
      | true,  true  -> (dst - src + ax, ar, true)::acc_r) [] acc_m) l_r map in
  List.filter_map (fun (x, r, _) -> if r > 0 then Some (x, r) else None) l_r

let () =
  let lines = read_lines in
  let seeds = List.nth lines 0 in
  let seeds = String.trim (List.nth (String.split_on_char ':' seeds) 1) in
  let seeds = String.split_on_char ' ' seeds in
  let seeds = List.map (fun s -> Scanf.sscanf s "%d" (fun p -> p)) seeds in
  let seeds = let rec comb l = match l with | x::y::l -> (x, y)::(comb l) | _ -> [] in comb seeds
  and maps = match lines with | _::_::_::l -> l | _ -> [] in
  let maps = List.fold_left (fun map_acc l ->
    if l = "" then
      []::map_acc
    else if is_num l.[0] then
      let nums = Scanf.sscanf l "%d %d %d" (fun a b c -> (a, b, c)) in
      match map_acc with
      | x::l -> ((nums::x)::l)
      | [] -> map_acc
    else map_acc) [[]] maps |> List.rev in
  let ranges = List.fold_left (fun acc map -> get_range_mapping acc map) seeds maps in
  let ret = List.fold_left min max_int (List.map fst ranges) in
  Printf.printf "%d\n" ret

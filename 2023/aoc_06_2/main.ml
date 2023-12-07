let read_lines =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop []

let read_nums line =
  let try_read s =
    try Some (Scanf.sscanf s " %d%n " (fun p n -> (p, n))) with End_of_file -> None in
  let rec loop acc s = match try_read s with
  | Some (m, n) -> loop (m :: acc) (String.sub s n ((String.length s) - n))
  | None -> List.rev acc in
  loop [] line

let f (t, d) =
  let a = t - Int.of_float (Float.sqrt (Float.of_int (t * t - 4 * (d + 1)))) in
  t + 1 - 2 * ((a + 1) / 2)

let parse s =
  read_nums (List.nth (String.split_on_char ':' s) 1)

let convert_l_n l =
  List.fold_left (fun acc n -> 
    acc * (Int.of_float (10. ** (1.+.(Float.floor (Float.log10 (Float.of_int n)))))) + n) 0 l

let () =
  let lines = read_lines in
  let times = parse (List.nth lines 0)
  and dist = parse (List.nth lines 1) in
  let t = convert_l_n times
  and d = convert_l_n dist in
  let ret = f (t, d) in
  Printf.printf "%d\n" ret

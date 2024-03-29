let read_lines =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop []

let is_num c =
  c >= '0' && c <= '9'

let num_to_int c =
  (Char.code c) - (Char.code '0')

let () =
  let lines = read_lines in
  let ret = List.fold_left (fun acc line ->
    let l = List.of_seq (String.to_seq line) in 
    let f_n = List.find is_num l in
    let l_n = List.find is_num (List.rev l) in
    acc + (num_to_int f_n) * 10 + (num_to_int l_n)) 0 lines in
  Printf.printf "%d\n"  ret

let hash s = 
  Seq.fold_left (fun acc c -> ((acc + Char.code c) * 17) mod 256) 0 (String.to_seq s)

let rec list_replace_assoc s n l =
  match l with
  | (k, v)::l ->
      if k = s then (k, n)::l 
      else (k, v)::(list_replace_assoc s n l)
  | [] -> []

let apply_instr boxes s =
  if s.[String.length s - 1] = '-' then
    let label = Scanf.sscanf s "%[a-z]-" Fun.id in
    let hash_label = hash label in
    let box = boxes.(hash_label) in
    boxes.(hash_label) <- List.remove_assoc label box
  else
    let label, n = Scanf.sscanf s "%[a-z]=%d" (fun s n -> s, n) in
    let hash_label = hash label in
    let box = boxes.(hash_label) in
    boxes.(hash_label) <-
      if List.mem_assoc label box then
        list_replace_assoc label n box
      else
        (label, n)::box

let () =
  let hash_instrs = String.split_on_char ',' (input_line stdin) in
  let boxes = Array.make 256 [] in
  List.iter (apply_instr boxes) hash_instrs;
  let boxes = Array.map List.rev boxes in
  let ret = Array.fold_left (fun (ai, i) l -> (ai + (List.fold_left (fun (aj, j) (_, n) ->
    (aj + (i * j * n), j + 1)) (0, 1) l |> fst), i + 1)) (0, 1) boxes |> fst in
  Printf.printf "%d\n" ret

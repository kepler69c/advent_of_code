let hash s = 
  Seq.fold_left (fun acc c -> ((acc + Char.code c) * 17) mod 256) 0 (String.to_seq s)

let () =
  let hash_instrs = String.split_on_char ',' (input_line stdin) in
  Printf.printf "%d\n" (List.fold_left ( + ) 0 (List.map (fun s -> hash s) hash_instrs))

let read_lines =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop []

let nums = [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine";
              "1";   "2";   "3";     "4";    "5";    "6";    "7";    "8";     "9" |]
let nums_c = Array.map (fun x -> List.of_seq (String.to_seq x)) nums
let nums_c_rev = Array.map List.rev nums_c
let nums_assoc = let n = Array.of_seq (Seq.take 9 (Seq.ints 1)) in Array.concat [n; n]

let consume_nums assoc c nc =
  Array.mapi (fun i ent -> match ent, nc.(i) with
  | x1::l1, x2::l2 ->
      if x1 == c then l1 else if x2 == c then l2 else nc.(i)
  | _ -> []) assoc

let find_num l nc = List.fold_left (fun a c ->
  match a with
  | Some e, _ -> a
  | None, a -> let a = consume_nums a c nc in 
      match (Array.find_opt (fun (l, n) -> l = []) (Array.combine a nums_assoc)) with
      | None -> (None, a)
      | Some (_, n) -> (Some n, a)) (None, Array.copy nc) l |> fst |> Option.get 

let () =
  let lines = read_lines in
  let ret = List.fold_left (fun acc line ->
    let l = List.of_seq (String.to_seq line) in 
    let fd = find_num l nums_c in
    let l_rev = List.rev l in
    let ld = find_num l_rev nums_c_rev in
    acc + fd * 10 + ld) 0 lines in
  Printf.printf "%d\n" ret

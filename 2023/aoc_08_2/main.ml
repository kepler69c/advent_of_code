let read_nodes () =
  let try_read () =
    try Some (Scanf.scanf "%[A-Z0-9] = (%[A-Z0-9], %[A-Z0-9])\n" (fun a b c -> (a, (b, c)))) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop [] |> Array.of_list

let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

module IS = Set.Make(Int)

let () =
  let instr = String.to_seq (Scanf.scanf "%[RL]\n\n" (fun p -> p)) in
  let nodes_str = read_nodes () in
  let map = Hashtbl.create 42 in
  Hashtbl.add_seq map (Seq.mapi (fun i x -> (x, i)) (Array.to_seq (Array.map fst nodes_str)));
  let nodes = Array.map (fun (_, (b, c)) -> (Hashtbl.find map b, Hashtbl.find map c)) nodes_str in
  let firsts = Hashtbl.fold (fun k v acc ->
    if k.[(String.length k) - 1] = 'A' then v::acc else acc) map [] |> Array.of_list in
  let lasts = Hashtbl.fold (fun k v acc ->
    if k.[(String.length k) - 1] = 'Z' then v::acc else acc) map [] |> IS.of_list in
  let rec find_loop i visited n loop seq seq_bak =
    if Seq.is_empty seq && Hashtbl.mem visited i then (Hashtbl.find visited i, List.rev loop)
    else
      let c, seq = match Seq.uncons seq with
      | Some x -> x
      | None -> Hashtbl.add visited i n; (Seq.uncons seq_bak) |> Option.get in 
      find_loop ((match c with | 'L' -> fst | _ -> snd) nodes.(i)) visited (n + 1) (i::loop) seq seq_bak in
  let loops = Array.map (fun first -> let v = Hashtbl.create 42 in find_loop first v 0 [] instr instr) firsts in
  let max_prelude = Array.fold_left (fun a (n, l) -> Int.max a n) 0 loops in
  let loops = Array.map (fun (n, l) -> 
     List.to_seq l |> Seq.drop n |> Seq.cycle |> Seq.take ((List.length l) - n) |> List.of_seq) loops in
  let system =
    Array.map (fun loop ->
      (List.find (fun (i, x) -> IS.mem x lasts) (List.mapi (fun i x -> (i, x)) loop) |> fst, List.length loop)) loops in
  let system = Array.map (fun (x, y) -> (y, x)) system in
  Array.sort compare system;
  let system = Array.map (fun (x, y) -> (y, x)) system in
  let rec find_in_system ax am x m s =
    if ax = x then s
    else find_in_system ((ax + am) mod m) am x m (s + am) in
  let ret = 
    (Array.fold_left (fun (ax, am) (x, m) -> 
      (find_in_system ax am x m ax, m * am / (gcd m am))) system.(0)
      (Array.sub system 1 (Array.length system - 1)) |> fst) + max_prelude in
  Printf.printf "%d\n" ret

let read_nodes () =
  let try_read () =
    try Some (Scanf.scanf "%[A-Z] = (%[A-Z], %[A-Z])\n" (fun a b c -> (a, (b, c)))) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop [] |> Array.of_list

let () =
  let instr = Seq.zip (Seq.ints 1) (Seq.cycle (String.to_seq (Scanf.scanf "%[RL]\n\n" (fun p -> p)))) in
  let nodes = read_nodes () in
  let map = Hashtbl.create 42 in
  Hashtbl.add_seq map (Seq.mapi (fun i x -> (x, i)) (Array.to_seq (Array.map fst nodes)));
  let nodes = Array.map (fun (_, (b, c)) -> (Hashtbl.find map b, Hashtbl.find map c)) nodes in
  let first = Hashtbl.find map "AAA" in
  let last = Hashtbl.find map "ZZZ" in
  let ret = ref first in
  let ret = Seq.find (fun (i, instr) ->
    ret := (match instr with
    | 'L' -> fst | 'R' -> snd | _ -> assert false) nodes.(!ret);
    !ret = last) instr |> Option.get |> fst in
  Printf.printf "%d\n" ret

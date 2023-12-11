let read_lines () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s :: acc)
  | None -> List.rev acc in
  loop []

let () =
  let lines = read_lines () in
  let numbers = List.map (fun s ->
    List.map (fun n ->
      Scanf.sscanf n "%d" (fun a -> a)) (String.split_on_char ' ' s)) lines in
  let generate_next l =
    let l_a = List.rev (List.tl (List.rev l)) in
    let l_b = List.tl l in
    List.map (fun (a, b) -> b - a) (List.combine l_a l_b) in
  let rec generate_all l =
    if List.for_all (Int.equal 0) l then [l]
    else l :: (generate_all (generate_next l)) in
  let predict ll =
    List.fold_left ( + ) 0 (List.map (fun l -> List.hd (List.rev l)) ll) in
  let ret = List.fold_left ( + ) 0 (List.map (fun l -> predict (generate_all l)) numbers) in
  Printf.printf "%d\n" ret

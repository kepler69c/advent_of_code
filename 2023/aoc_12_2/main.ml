type spring = | Ok | Damaged | Unknown

let read_springs () =
  let try_read () =
    try Some (Scanf.scanf "%s %s\n" (fun a b -> (a, b))) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some (springs, damage_list) ->
      let springs = List.map (fun c -> match c with | '.' -> Ok | '#' -> Damaged | '?' -> Unknown | _ -> assert false)
      (String.to_seq springs |> List.of_seq) in
      let damage_list = String.split_on_char ',' damage_list |> List.map (fun s -> Scanf.sscanf s "%d" Fun.id) in
      loop ((springs, damage_list) :: acc)
  | None -> List.rev acc in
  loop [] |> Array.of_list

let print_springs springs =
  List.iter (fun s -> Printf.printf "%c" (match s with | Ok -> '.' | Damaged -> '#' | Unknown -> '?')) springs

let rec try_fit springs damage_list memo =
  match Hashtbl.find_opt memo (springs, damage_list) with
  | Some n -> n
  | None ->
    (let n = match springs with
    | Ok::springs ->
        try_fit springs damage_list memo
    | Damaged::springs ->
        (match damage_list with
        | damage::damage_list ->
            let seq = List.to_seq springs in
            (if Seq.for_all (fun c -> c <> Ok) (Seq.take (damage - 1) seq) && Seq.length seq >= (damage - 1) then 
              match (Seq.drop (damage - 1) seq |> List.of_seq) with
              | [] -> try_fit [] damage_list memo
              | Ok::springs
              | Unknown::springs ->  
                  try_fit springs damage_list memo
              | _ -> 0
            else 0)
        | _ -> 0)
    | Unknown::springs ->
        (try_fit (Ok::springs) damage_list memo) + (try_fit (Damaged::springs) damage_list memo)
    | [] -> if damage_list = [] then 1 else 0 in
    Hashtbl.replace memo (springs, damage_list) n;
    n)

let expand_springs springs =
  List.to_seq (Unknown::springs) |> Seq.cycle |> Seq.take ((List.length springs + 1) * 5) |> List.of_seq |> List.tl

let repeat_five_list l =
  List.to_seq l |> Seq.cycle |> Seq.take (List.length l * 5) |> List.of_seq

let () =
  let spring_problem = read_springs () in
  let ret = Array.fold_left (fun a (springs, damage_list) -> 
    let memo = Hashtbl.create 42 in
    (try_fit (expand_springs springs) (repeat_five_list damage_list) memo) + a) 0 spring_problem in
  Printf.printf "%d\n" ret

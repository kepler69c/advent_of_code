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

let rec try_fit springs damage_list =
  match springs with
  | Ok::springs ->
      try_fit springs damage_list
  | Damaged::springs ->
      (match damage_list with
      | damage::damage_list ->
          let seq = List.to_seq springs in
          (if Seq.for_all (fun c -> c <> Ok) (Seq.take (damage - 1) seq) && Seq.length seq >= (damage - 1) then 
            match (Seq.drop (damage - 1) seq |> List.of_seq) with
            | [] -> try_fit [] damage_list
            | Ok::springs
            | Unknown::springs ->  
                try_fit springs damage_list
            | _ -> 0
          else 0)
      | _ -> 0)
  | Unknown::springs ->
      (try_fit (Ok::springs) damage_list) + (try_fit (Damaged::springs) damage_list)
  | [] -> if damage_list = [] then 1 else 0

let () =
  let spring_problem = read_springs () in
  let ret = Array.fold_left (fun a (springs, damage_list) -> (try_fit springs damage_list) + a) 0 spring_problem in
  Printf.printf "%d\n" ret

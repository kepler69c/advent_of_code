type operation = Less | Greater

let read_work () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some "" -> List.rev acc
  | Some s -> 
      let w, r = Scanf.sscanf s "%[a-z]{%[^}]" (fun w r -> (w, r)) in
      let rs = String.split_on_char ',' r in
      let rl = List.map (fun r -> 
        if String.contains r ':' then
          let c, g = String.split_on_char ':' r |> fun s -> (List.nth s 0, List.nth s 1) in
          (Some (if String.contains c '>' then
            Scanf.sscanf c "%c>%d" (fun c v -> (c, v, Greater))
          else Scanf.sscanf c "%c<%d" (fun c v -> (c, v, Less))), g)
        else (None, r)) rs in
      loop ((w, rl)::acc)
  | None -> assert false in
  loop []

let read_parts () =
  let try_read () =
    try Some (Scanf.scanf "{x=%d,m=%d,a=%d,s=%d}\n" (fun x m a s -> (x, m, a, s))) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop (s::acc)
  | None -> List.rev acc in
  loop []

let rec accept_reject work (x, m, a, s) work_table =
  match work with
  | "A" -> true
  | "R" -> false
  | work ->
      let work = List.find (fun (r, w) ->
        match r with
        | Some (c, v, o) ->
            let c = (match c with
            | 'x' -> x | 'm' -> m | 'a' -> a | 's' -> s | _ -> assert false) in
            (match o with
            | Greater -> c > v
            | Less -> c < v)
        | None -> true) (Hashtbl.find work_table work) |> snd in
      accept_reject work (x, m, a, s) work_table

let () =
  let work = read_work () in
  let parts = read_parts () in
  let work_table = Hashtbl.create 42 in
  Hashtbl.add_seq work_table (List.to_seq work);
  let accepted_parts = List.filter_map (fun (x, m, a, s) ->
    if accept_reject "in" (x, m, a, s) work_table then Some (x, m, a, s) else None) parts in
  let ret = List.fold_left (fun acc (x, m, a, s) -> acc + x + m + a + s) 0 accepted_parts in
  Printf.printf "%d\n" ret

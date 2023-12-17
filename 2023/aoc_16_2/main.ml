let read_mirrors () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec loop acc = match try_read () with
  | Some s -> loop ((String.to_seq s |> Array.of_seq) :: acc)
  | None -> List.rev acc in
  loop [] |> Array.of_list

let dims grid =
  let size_y = Array.length grid in
  if size_y > 0 then
    let size_x = Array.length grid.(0) in
    size_x, size_y
  else 0, 0

type direction = N | S | E | W

let rec progress_beams mirrors beams trace =
  let dim_x, dim_y = dims mirrors in
  let beams = List.fold_left (fun acc ((x, y), dir) ->
    if x < 0 || x >= dim_x || y < 0 || y >= dim_y || List.mem dir trace.(y).(x) then acc else
    (trace.(y).(x) <- dir :: trace.(y).(x);
    match mirrors.(y).(x), dir with
    | ('.'|'-'), E
    | '/', N
    | '\\', S -> ((x + 1, y), E) :: acc
    | ('.'|'-'), W
    | '/', S
    | '\\', N -> ((x - 1, y), W) :: acc
    | ('.'|'|'), N
    | '/', E
    | '\\', W -> ((x, y - 1), N) :: acc
    | ('.'|'|'), S
    | '/', W
    | '\\', E -> ((x, y + 1), S) :: acc
    | '|', (E|W) -> ((x, y - 1), N) :: ((x, y + 1), S) :: acc
    | '-', (N|S) -> ((x - 1, y), W) :: ((x + 1, y), E) :: acc
    | _, _ -> assert false)) [] beams in
  if beams <> [] then progress_beams mirrors beams trace
  else trace

let trace_beams mirrors =
  let dim_x, dim_y = dims mirrors in
  let starts = 
    (Seq.ints 0 |> Seq.take dim_x |> Seq.fold_left (fun acc x -> ((x, 0), S) :: ((x, dim_x - 1), N) :: acc) []) @
    (Seq.ints 0 |> Seq.take dim_y |> Seq.fold_left (fun acc y -> ((0, y), E) :: ((dim_y - 1, y), W) :: acc) []) in
  List.map (fun start -> progress_beams mirrors [start] (Array.(make dim_y [||] |> map (fun _ -> Array.make dim_x [])))) starts

let () =
  let mirrors = read_mirrors () in
  let traces = trace_beams mirrors in
  let traces = List.map (Array.map (Array.map (fun e -> e <> []))) traces in
  let rets =
    List.map (Array.fold_left (fun acc a -> acc + Array.fold_left (fun acc e -> acc + if e then 1 else 0) 0 a) 0) traces in
  let ret = List.fold_left max 0 rets in
  Printf.printf "%d\n" ret

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

let rec accept_reject work_xmas accepted_xmas work_table =
  let accepted = List.filter (fun (w, _) -> w = "A") work_xmas in
  let work_xmas = List.filter (fun (w, _) -> w <> "A" && w <> "R") work_xmas in
  if work_xmas = [] then (accepted_xmas @ accepted) else
  let work_xmas = List.fold_left (fun work_xmas (w1, (x, m, a, s)) ->
    List.fold_left (fun acc (r, w2) ->
      match List.find_opt (fun (w1', _) -> w1 = w1') acc with
      | None -> []
      | Some (w1, (x, m, a, s)) ->
          let other_works = List.filter (fun (w1', _) -> w1' <> w1) acc in
          other_works @ match r with
          | Some (c, v, o) ->
              let (cmin, cmax) = (match c with
              | 'x' -> x | 'm' -> m | 'a' -> a | 's' -> s | _ -> assert false) in
              let new_ranges = match o with
              | Greater ->
                  if cmax <= v then [] 
                  else [((cmin, v), w1); ((v + 1, cmax), w2)]
              | Less -> 
                  if cmin >= v then [] 
                  else [((cmin, v - 1), w2); ((v, cmax), w1)] in
              ((match c with
              | 'x' -> List.map (fun (x, w) -> (w, (x, m, a, s)))
              | 'm' -> List.map (fun (m, w) -> (w, (x, m, a, s)))
              | 'a' -> List.map (fun (a, w) -> (w, (x, m, a, s)))
              | 's' -> List.map (fun (s, w) -> (w, (x, m, a, s)))
              | _ -> assert false) new_ranges)
          | None -> [(w2, (x, m, a, s))])
    [(w1, (x, m, a, s))] (Hashtbl.find work_table w1) @ work_xmas) [] work_xmas in
    accept_reject work_xmas (accepted_xmas @ accepted) work_table

type tree = Node of (((int * int) * tree) list) | Leaf of bool
type merge_state = Equals | Contains | In | Left | LeftIn | Right | RightIn | Out

let print_tree tree =
  let rec loop tree height =
    match tree with
    | Leaf(true) -> Printf.printf "Leaf(true)"
    | Leaf(false) -> Printf.printf "Leaf(false)"
    | Node(l) ->
        List.iter (fun ((b, e), tree) ->
          Printf.printf "\n%s(%d, %d) -> " (String.of_seq (Seq.(repeat ' ' |> take height))) b e;
          loop tree (height + 1)) l in
  loop tree 0;
  Printf.printf "\n"

let build_accept_reject_tree accepted_works =
  let tree = Leaf(true) in
  let rec build tree l_i c_i pred =
    let get_xmas i xmas =
      match xmas with
      | (x, _, _ ,_) when i = 0 -> x
      | (_, m, _ ,_) when i = 1 -> m
      | (_, _, a ,_) when i = 2 -> a
      | (_, _, _ ,s) -> s in
    let range_merge_state r1 r2 =
      let b1, e1 = r1 and b2, e2 = r2 in
      if b1 > e2 || e1 < b2 then Out else
      match b1 = b2, e1 = e2, b1 < b2, e1 < e2 with
      | true, true, _, _ -> Equals
      | false, false, true, false 
      | false, true, true, _ 
      | true, false, _, false -> Contains
      | _, true, false, _ -> RightIn
      | true, _, _, true -> LeftIn
      | _, _, false, false -> Right
      | _, _, false, true -> In
      | _, _, true, true -> Left in
    if c_i >= 4 || tree = Leaf(false) then tree
    else if l_i >= (Array.length accepted_works) then
      match tree with
      | Leaf(b) -> Leaf(b)
      | Node(l) -> Node(List.map (fun (r, tree) ->
          (r, build tree 0 (c_i + 1) (r :: pred))) l)
    else match tree with
    | Leaf(b) -> build (Node([((1,4000), Leaf(false))])) l_i c_i pred
    | Node(l) ->
        if not (List.fold_left (fun (i, acc) range_pred ->
          let range = get_xmas i accepted_works.(l_i) in
          (i - 1, acc && (match range_merge_state range range_pred with
          | Contains | Equals -> true | _ -> false))) (c_i - 1, true) pred |> snd)
        then build tree (l_i + 1) c_i pred else
        let range = get_xmas c_i accepted_works.(l_i) in
        build (Node(List.fold_left (fun acc (range_tree, tree) ->
          let br, er = range and brt, ert = range_tree in
          acc @ match range_merge_state range range_tree with
          | Equals | Contains -> [(range_tree, Leaf(true))]
          | Out -> [(range_tree, tree)]
          | In -> [((brt, br - 1), tree); ((br, er), Leaf(true)); ((er + 1, ert), tree)]
          | LeftIn -> [((br, er), Leaf(true)); ((er + 1, ert), tree)]
          | RightIn -> [((brt, br - 1), tree); ((br, er), Leaf(true))]
          | Left -> [((brt, er), Leaf(true)); ((er + 1, ert), tree)]
          | Right -> [((brt, br - 1), tree); ((br, ert), Leaf(true))]) [] l)) (l_i + 1) c_i pred
    in
  build tree 0 0 []

let compute_tree tree =
  let rec loop tree acc =
    match tree with
    | Leaf(true) -> acc
    | Leaf(false) -> 0
    | Node(l) -> List.fold_left (fun res ((b, e), tree) ->
        res + loop tree (acc * (e - b + 1))) 0 l in
  loop tree 1

let compute_accept accepted_works =
  let tree = build_accept_reject_tree accepted_works in
  compute_tree tree

let () =
  let work = read_work () in
  let parts = ((1, 4000), (1, 4000), (1, 4000), (1, 4000)) in
  let work_table = Hashtbl.create 42 in
  Hashtbl.add_seq work_table (List.to_seq work);
  let accepted_works = accept_reject [("in", parts)] [] work_table in
  Printf.printf "accepted_works:\n";
  List.iter (fun (w, ((x_s, x_e), (m_s, m_e), (a_s, a_e), (s_s, s_e))) ->
    Printf.printf "w: %s, x:[%d,%d], m:[%d,%d], a:[%d,%d], s:[%d,%d]\n"
    w x_s x_e m_s m_e a_s a_e s_s s_e) accepted_works;
  let accepted_works = Array.of_list accepted_works |> Array.map snd in
(*   let ret = compute_accept accepted_works in *)
  let ret = compute_accept accepted_works in
  Printf.printf "%d\n" ret

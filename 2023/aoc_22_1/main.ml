let read_bricks () =
  let try_read () =
    try Some (Scanf.scanf "%d,%d,%d~%d,%d,%d\n" 
    (fun n11 n12 n13 n21 n22 n23 -> ((n11, n12, n13), (n21, n22, n23))))
    with End_of_file -> None in
  let rec loop acc = 
    match try_read () with
    | Some s -> s::(loop acc)
    | None -> List.rev acc in
  loop []

let overlap_line s1 s2 =
  let x1, x2 = s1 in
  let x1', x2' = s2 in
  not ((max x1 x2) < (min x1' x2') || (min x1 x2) > (max x1' x2'))

let overlap b1 b2 =
  let ((x11, y11, _), (x12, y12, _)) = b1 in
  let ((x21, y21, _), (x22, y22, _)) = b2 in
  (overlap_line (x11, x12) (x21, x22)) && (overlap_line (y11, y12) (y21, y22))

let gravity bricks =
  let sort_falling = List.sort (fun b1 b2 ->
    let ((_, _, z1), (_, _, z2)) = b1 in
    let ((_, _, z1'), (_, _, z2')) = b2 in
    let mz, mz' = min z1 z2, min z1' z2' in
    if mz = mz' then compare b1 b2 else compare mz mz') bricks in
  let fallen_bricks =
    List.fold_left (fun acc b ->
      let ((x1, y1, z1), (x2, y2, z2)) = b in
      if (min z1 z2) = 1 then b :: acc else
      let below_bricks = 
        List.filter (fun b' ->
          let ((_, _, z1'), (_, _, z2')) = b' in
          (max z1' z2') = (min z1 z2) - 1) acc in
      if List.for_all (fun b' -> not (overlap b' b)) below_bricks then
        ((x1, y1, z1 - 1), (x2, y2, z2 - 1)) :: acc else
        b :: acc) [] sort_falling in
  fallen_bricks

let rec fixpoint_gravity bricks =
  let bricks' = bricks in
  let fallen_bricks = gravity bricks in
  if bricks' = fallen_bricks then bricks'
  else (fixpoint_gravity fallen_bricks)

let () =
  let bricks = read_bricks () in
  let ground_bricks = fixpoint_gravity bricks in
  let ret = List.fold_left (fun acc b ->
    let rem_bricks = List.filter ((<>) b) ground_bricks in
    acc + (if rem_bricks = (gravity rem_bricks) then 1 else 0)) 0 ground_bricks in
  Printf.printf "%d\n" ret

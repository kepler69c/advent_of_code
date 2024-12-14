import System.Environment (getArgs)
import Data.List.Split

parse path = do
  c <- readFile path
  let l = lines c
      ll = splitOn [""] l
      rl = map (\[a, b, p] ->
        let as = words a
            ax = read $ init $ drop 2 $ as !! 2
            ay = read $ drop 2 $ as !! 3 in
        let bs = words b
            bx = read $ init $ drop 2 $ bs !! 2
            by = read $ drop 2 $ bs !! 3 in
        let ps = words p
            px = read $ init $ drop 2 $ ps !! 1
            py = read $ drop 2 $ ps !! 2 in
        ((ax, ay), (bx, by), (px + 10000000000000, py + 10000000000000))) ll
  return rl

vecMul a = map (* a)

vecSub = zipWith (-)

solve [[m11, m12], [m21, m22]] [p1, p2] =
  let [m211, m221, p21] = vecSub [m21, m22, p2] (vecMul (m21 / m11) [m11, m12, p1])
      [m112, m122, p12] = vecSub [m11, m12, p1] (vecMul (m12 / m221) [m211, m221, p21])
      [m113, m123, p13] = vecMul (1 / m112) [m112, m122, p12]
      [m214, m224, p24] = vecMul (1 / m221) [m211, m221, p21] in
  [p13, p24]

check l =
  let [a, b, ax, ay, bx, by, px, py] = map round l in
  (a * ax + b * bx, a * ay + b * by) == (px, py)

f = sum .
    map (\((ax, ay), (bx, by), (px, py)) ->
      let [a, b] = solve [[ax, bx], [ay, by]] [px, py] in
      if check [a, b, ax, ay, bx, by, px, py] then 3 * round a + round b
      else 0)

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

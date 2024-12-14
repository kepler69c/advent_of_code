import System.Environment (getArgs)
import Data.List.Split

parse :: FilePath -> IO [((Double, Double), (Double, Double), (Double, Double))]
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
        ((ax, ay), (bx, by), (px, py))) ll
  return rl

vec3_mul a [v1, v2, v3] =
  [a * v1, a * v2, a * v3]

vec3_sub [v1, v2, v3] [v4, v5, v6] =
  [v1 - v4, v2 - v5, v3 - v6]

solve [[m11, m12], [m21, m22]] [p1, p2] =
  let [m21_1, m22_1, p2_1] = vec3_sub [m21, m22, p2] (vec3_mul (m21 / m11) [m11, m12, p1])
      [m11_2, m12_2, p1_2] = vec3_sub [m11, m12, p1] (vec3_mul (m12 / m22_1) [m21_1, m22_1, p2_1])
      [m11_3, m12_3, p1_3] = vec3_mul (1/m11_2) [m11_2, m12_2, p1_2]
      [m21_4, m22_4, p2_4] = vec3_mul (1/m22_1) [m21_1, m22_1, p2_1] in
  [p1_3, p2_4]

check l =
  let [a, b, ax, ay, bx, by, px, py] = map round l in
  (a * ax + b * bx, a * ay + b * by) == (px, py)

f :: [((Double, Double), (Double, Double), (Double, Double))] -> Int
f = sum . map (\((ax, ay), (bx, by), (px, py)) ->
            let [a, b] = solve [[ax, bx], [ay, by]] [px, py] in
            if check [a, b, ax, ay, bx, by, px, py] then
              3 * round a + round b
            else 0)

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

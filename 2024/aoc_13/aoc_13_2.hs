import System.Environment (getArgs)
import Data.List.Split

parse :: FilePath -> IO [((Integer, Integer), (Integer, Integer), (Integer, Integer))]
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
        ((ax, ay), (bx, by), (10000000000000 + px, 10000000000000 + py))) ll
  return rl

-- this is fixed point algebra
scf = 100000000000000000000
err = 10000000000000

sc a = a * scf

dc a =
  let r = a `mod` scf
      d = (a `div` scf) + (if r - (scf `div` 2) <= 0 then 0 else 1) in
  if abs (d * scf - a) > err then Nothing else Just d

vec3_mul a [v1, v2, v3] =
  [(a * v1) `div` scf, (a * v2) `div` scf, (a * v3) `div` scf]

vec3_div a [v1, v2, v3] =
  [(v1 * scf) `div` a, (v2 * scf) `div` a, (v3 * scf) `div` a]

vec3_sub [v1, v2, v3] [v4, v5, v6] =
  [v1 - v4, v2 - v5, v3 - v6]

solve [m11, m12, p1] [m21, m22, p2] =
  let [m21_1, m22_1, p2_1] = vec3_sub [m21, m22, p2] (vec3_div m11 (vec3_mul m21 [m11, m12, p1]))
      [m11_2, m12_2, p1_2] = vec3_sub [m11, m12, p1] (vec3_div m22_1 (vec3_mul m12 [m21_1, m22_1, p2_1]))
      [m11_3, m12_3, p1_3] = vec3_div m11_2 [m11_2, m12_2, p1_2]
      [m21_4, m22_4, p2_4] = vec3_div m22_1 [m21_1, m22_1, p2_1] in
  [p1_3, p2_4]

f :: [((Integer, Integer), (Integer, Integer), (Integer, Integer))] -> Integer
f = sum . map (\((ax, ay), (bx, by), (px, py)) ->
            let [ax', bx', px'] = map sc [ax, bx, px]
                [ay', by', py'] = map sc [ay, by, py]
                [a', b'] = solve [ax', bx', px'] [ay', by', py'] in
            case (dc a', dc b') of
            (Just a, Just b) -> a * 3 + b
            _ -> 0)

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

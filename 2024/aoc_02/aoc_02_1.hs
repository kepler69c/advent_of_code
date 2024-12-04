import System.Environment (getArgs)

parse path = do
  c <- readFile path
  let l = lines c
      ll = map (map read . words) l
  return ll

f' (a : l) = zipWith (-) (a : l) l

f ll =
  let ll' = map f' ll
      ll1' = map (\l -> all (> 0) l || all (< 0) l) ll'
      ll2' = map (all ((\x -> x >= 1 && x <= 3) . abs)) ll' in
  length $ filter id $ zipWith (&&) ll1' ll2'

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

import System.Environment (getArgs)

parse path = do
    c <- readFile path
    let l = lines c
        ll = map (map read . words) l
    return ll

f' l =
  let a:l' = l
      l'' = snd $ foldl (\(a, l) b -> (b, (a - b):l)) (a, []) l' in
  l''

f ll =
  let ll' = map f' ll
      ll'' = map (\l -> all (> 0) l || all (< 0) l) ll'
      ll''' = map (all ((\x -> x >= 1 && x <= 3) . abs)) ll' in
  length (filter id (zipWith (&&) ll'' ll'''))

main = do
  (a:_) <- getArgs
  l <- parse a
  print (f l)

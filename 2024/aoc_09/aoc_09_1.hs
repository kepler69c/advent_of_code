import System.Environment (getArgs)

parse path = do
  c <- readFile path
  let (a : _) = lines c
  return (map (read . (: [])) a)

diskEmpty [] _ = []
diskEmpty (a : l) i =
  replicate a Nothing ++ disk l i

disk [] _ = []
disk (a : l) i =
  replicate a (Just i) ++ diskEmpty l (i + 1)

compact [] = []
compact d =
  case (d, reverse d) of
  (Nothing : _, Nothing : l2) -> compact (reverse l2)
  (Nothing : _, Just e2 : l2) -> e2 : compact (tail (reverse l2))
  (Just e1 : l1, _) -> e1 : compact l1

f l =
  foldr (\(i, e) s -> i * e + s) 0 (zip [0..] (compact (disk l 0)))

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

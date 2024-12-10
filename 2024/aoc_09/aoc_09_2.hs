import System.Environment (getArgs)

parse path = do
  c <- readFile path
  let (a : _) = lines c
  return (map (read . (: [])) a)

data Block = Empty Int | File Int Int deriving Eq

diskEmpty [] _ = []
diskEmpty (n : l) i =
  Empty n : disk l i

disk [] _ = []
disk (n : l) i = File n i : diskEmpty l (i + 1)

splitEmpty r ((Empty n1) : l) (File n2 e)
  | n1 < n2 = splitEmpty (Empty n1 : r) l (File n2 e)
  | n1 == n2 = Just (reverse r, [File n2 e], l) -- this line can be removed, improves time
  | otherwise = Just (reverse r, [File n2 e, Empty (n1 - n2)], l)
splitEmpty r (e : l) f = splitEmpty (e : r) l f
splitEmpty _ [] _ = Nothing

compact mutD [] = mutD
compact mutD (Empty _ : revD) = compact mutD revD
compact mutD (File n2 e : revD) =
  let (l3, _ : l4) = break (== File n2 e) mutD in
  case splitEmpty [] l3 (File n2 e) of
  Nothing -> compact mutD revD
  Just (l1, l5, l2) -> compact (l1 ++ l5 ++ l2 ++ [Empty n2] ++ l4) revD

readCompact (Empty n : c) = replicate n 0 ++ readCompact c
readCompact (File n e : c) = replicate n e ++ readCompact c
readCompact [] = []

f l =
  let d = disk l 0
      c = compact d (reverse d) in
  foldr (\(i, e) s -> i * e + s) 0 (zip [0..] (readCompact c))

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

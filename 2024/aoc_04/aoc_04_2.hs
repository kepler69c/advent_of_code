import System.Environment (getArgs)

parse path = do
  c <- readFile path
  let l = lines c
  return l

isXmas [l1, l2, l3] =
  case (l1, l2, l3) of
  (['M',  _ , 'M'], [ _ , 'A',  _ ], ['S',  _ , 'S']) -> True
  (['M',  _ , 'S'], [ _ , 'A',  _ ], ['M',  _ , 'S']) -> True
  (['S',  _ , 'M'], [ _ , 'A',  _ ], ['S',  _ , 'M']) -> True
  (['S',  _ , 'S'], [ _ , 'A',  _ ], ['M',  _ , 'M']) -> True
  _ -> False

matrixWindow n ll =
  if length ll < n then []
  else matrixWindowRow (take n ll) ++ matrixWindow n (drop 1 ll)
  where
  matrixWindowRow (h : l) =
    if length h < n then []
    else map (take n) (h : l) : matrixWindowRow (map (drop 1) (h : l))

f l =
  let windows = matrixWindow 3 l in
  length $ filter id (map isXmas windows)

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)
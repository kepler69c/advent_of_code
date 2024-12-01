import System.Directory.Internal.Prelude (getArgs)
import Data.List (sort)

parse path = do
    c <- readFile path
    let l = lines c
        ll = map (map read . words) l
        (l1,l2) = unzip $ map (\(x:y:_) -> (x,y)) ll
    return (l1,l2)

f (l1,l2) =
  let sl1 = sort l1
      sl2 = sort l2
      d = map (\(x, y) -> abs (x - y)) (zip sl1 sl2)
      s = sum d in
      s

main = do
  (a:_) <- getArgs
  l <- parse a
  print (f l)

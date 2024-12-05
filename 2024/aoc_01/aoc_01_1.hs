import System.Environment (getArgs)
import Data.List (sort)

parse path = do
  c <- readFile path
  let l = lines c
      ll = map (map read . words) l
      (l1, l2) = unzip $ map (\(x : y : _) -> (x, y)) ll
  return (l1, l2)

f (l1, l2) = sum $ zipWith (\x y -> abs (x - y)) (sort l1) (sort l2)

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

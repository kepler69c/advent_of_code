import System.Environment (getArgs)
import Data.Array.IArray
import Data.Bifunctor (Bifunctor(bimap))

parse path = do
  c <- readFile path
  let (a : l) = lines c
      dy = length (a : l)
      dx = length a
      arr = listArray ((1, 1), (dy, dx)) (map (read . (: [])) (concat (a : l)))
  return arr

neighbors = [(-1, 0), (1, 0), (0, 1), (0, -1)]

inBounds a (y, x) =
  let ((ly, lx), (uy, ux)) = bounds a in
  (y >= ly) && (y <= uy) && (x >= lx) && (x <= ux)

dfs :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
dfs a (y, x) =
  let e = a ! (y, x) in
  if e == 9 then [(y, x)] else
  let n = filter (\i -> inBounds a i && (a ! i) == (e + 1)) $ map (bimap (+ y) (+ x)) neighbors in
  foldr (\i s -> s ++ dfs a i) [] n

f a =
  let starts = filter (\i -> (a ! i) == 0) (indices a) in
  sum $ map (length . filter (== 9) . map (a !) . dfs a) starts

main = do
  (a : _) <- getArgs
  arr <- parse a
  print (f arr)

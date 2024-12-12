{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Data.Array.IArray
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (find)

parse :: FilePath -> IO (Array (Int, Int) Char)
parse path = do
  c <- readFile path
  let (h : l) = lines c
      dx = length h
      dy = length (h : l)
      a = listArray ((1, 1), (dx, dy)) (concat (h : l))
  return a

neighbors = [(0, 1), (1, 0), (0, -1), (-1, 0)]

inBounds a (y, x) =
  let ((ly, lx), (uy, ux)) = bounds a in
  (y >= ly) && (y <= uy) && (x >= lx) && (x <= ux)

dfs :: Array (Int, Int) (Bool, Char) -> Char -> (Int, Int) -> ([(Int, Int)], Array (Int, Int) (Bool, Char))
dfs a c (y, x) =
  let n = filter (inBounds a) $ map (bimap (+ y) (+ x)) neighbors in
  foldr (\i (s, a) ->
    if (a ! i) == (True, c) then
      let (s', a') = dfs (a // [(i, (False, c))]) c i in
      (s ++ s', a')
    else (s, a)) ([(y, x)], a) n

collectFields a =
  let ba = amap (True,) a in
  collectF ba
  where
  collectF :: Array (Int, Int) (Bool, Char) -> [([(Int, Int)], Char)]
  collectF ba =
    case find (fst . snd) (assocs ba) of
    Just (p, (_, c)) ->
      let (f, ba') = dfs (ba // [(p, (False, c))]) c p in
      (f, c) : collectF ba'
    Nothing -> []

computeAP a (ps, c) =
  let area = length ps
      perimeter = length $ 
                  concatMap (\(y, x) ->
                    filter not $
                    map (\(oy, ox) ->
                      let i' = (oy + y, ox + x) in
                      (inBounds a i' && (a ! i') == c)) neighbors) ps in
  (area, perimeter)

f a =
  let (areas, perimeters) = unzip $ map (computeAP a) (collectFields a) in
  sum $ zipWith (*) areas perimeters

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

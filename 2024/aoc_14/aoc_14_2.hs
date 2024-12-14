{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Data.List.Split
import Data.Array.IArray
import Data.List (intercalate, transpose)

parse path = do
  c <- readFile path
  let l = lines c
      ll = map (map (drop 2) . words) l
      lll = map ((\[a, b] -> (a, b)) . map ((\[a, b] -> (a, b)) . map read . splitOn ",")) ll
  return lll

step (dx, dy) =
  map (\((x, y), (vx, vy)) ->
    (((x + vx) `mod` dx, (y + vy) `mod` dy), (vx, vy)))

inBounds ((dxl, dyl), (dxu, dyu)) (x, y) =
  x >= dxl && x <= dxu && y >= dyl && y <= dyu

display a (dx, dy) = do
  let arr = listArray ((1, 1), (dy, dx)) (repeat ' ') :: Array (Int, Int) Char
      arr' = arr // map ((, '#') . (\(x, y) -> (y + 1, x + 1)) . fst) a
      l = intercalate "\n" $ chunksOf dx (elems arr')
  putStrLn l

f a = do
  let (dx, dy) = (101, 103)
      a' = iterate (step (dx, dy)) a !! 7037 -- found by hand
  print 7037
  display a' (dx, dy)

main = do
  (a : _) <- getArgs
  l <- parse a
  f l

import System.Environment (getArgs)
import Data.List.Split
import Data.Array.IArray
import Debug.Trace (trace)
import Data.List (intercalate)
import Control.Monad (when)

parse :: FilePath -> IO [((Int, Int), (Int, Int))]
parse path = do
  c <- readFile path
  let l = lines c
      ll = map (map (drop 2) . words) l
      lll = map ((\[a, b] -> (a, b)) . map ((\[a, b] -> (a, b)) . map read . splitOn ",")) ll
  return lll

step (dx, dy) =
  map (\((x, y), (vx, vy)) ->
    (((x + vx) `mod`dx, (y + vy) `mod` dy), (vx, vy)))

inBounds ((dxl, dyl), (dxu, dyu)) (x, y) =
  x >= dxl && x <= dxu && y >= dyl && y <= dyu

display a (dx, dy) = do
  let arr = listArray ((1, 1), (dx, dy)) (repeat ' ') :: Array (Int, Int) Char
      arr' = arr // zip (map ((\(x, y) -> (x + 1, y + 1)). fst) a) (repeat '#')
      l = intercalate "\n" $ chunksOf dx (elems arr')
  putStrLn l

confidence a (dx, dy) =
  let arr = listArray ((1, 1), (dx, dy)) (repeat False) :: Array (Int, Int) Bool
      arr' = arr // zip (map ((\(x, y) -> (x + 1, y + 1)). fst) a) (repeat True) in
  any ((>= 31) . length . filter id) $ chunksOf dx (elems arr')

f :: [((Int, Int), (Int, Int))] -> Int -> IO [((Int, Int), (Int, Int))]
f a i = do
  let (dx, dy) = (101, 103)
      a' = step (dx, dy) a
  when (confidence a (dx, dy))
    (do
     print i
     display a (dx, dy))
  f a' (i + 1)

main = do
  (a : _) <- getArgs
  l <- parse a
  f l 0

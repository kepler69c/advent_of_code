import System.Environment (getArgs)
import Data.Text (count, pack)
import Data.List
import Data.Array (listArray, (!))
import Data.Maybe (isNothing, fromJust)

parse path = do
  c <- readFile path
  let l = lines c
  return l

cntXmas = count (pack "XMAS")

splitMaybe s =
  case dropWhile isNothing s of
  [] -> []
  s' -> map fromJust w : splitMaybe s''
        where (w, s'') = break isNothing s'

diagI' (ix, iy) b =
  let (nIx, nIy) = (ix + 1, iy - 1) in
  Just (ix, iy) : (case (nIx > b, nIy < 1) of
                   (True, _) -> Nothing : diagI' (nIy + 2, b) b
                   (_, True) -> Nothing : diagI' (1, nIx) b
                   _ -> diagI' (nIx, nIy) b)

diagI b = take (2 * b - 1) $ splitMaybe $ diagI' (1, 1) b

diag ll =
  let diagI_ = diagI (length ll) in
  let matrix = listArray ((1, 1), (length ll, length ll)) (concat ll) in
  map (map (matrix !)) diagI_

f ll =
  let horz_n = sum $ map (cntXmas . pack) ll
      horz_r = sum $ map (cntXmas . pack . reverse) ll
      vert_n = sum $ map (cntXmas . pack) $ transpose ll
      vert_r = sum $ map (cntXmas . pack . reverse) $ transpose ll
      diag1_n = sum $ map (cntXmas . pack) $ diag ll
      diag1_r = sum $ map (cntXmas . pack . reverse) $ diag ll
      diag2_n = sum $ map (cntXmas . pack) $ diag (map reverse ll)
      diag2_r = sum $ map (cntXmas . pack . reverse) $ diag (map reverse ll) in
  horz_n + horz_r + vert_n + vert_r + diag1_n + diag1_r + diag2_n + diag2_r

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

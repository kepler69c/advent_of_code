import System.Environment (getArgs)
import Data.List.Split

parse :: FilePath -> IO [((Int, Int), (Int, Int))]
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

f a =
  let (dx, dy) = (101, 103)
      a' = iterate (step (dx, dy)) a  !! 100
      q = [((0, 0), (49, 50)),
           ((51, 0), (100, 50)),
           ((0, 52), (49, 102)),
           ((51, 52), (100, 102))] in
  product $ map (length . (\q -> filter (inBounds q) (map fst a'))) q

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

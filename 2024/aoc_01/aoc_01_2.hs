import System.Environment (getArgs)
import qualified Data.Map as M

parse path = do
  c <- readFile path
  let l = lines c
      ll = map (map read . words) l
      (l1, l2) = unzip $ map (\(x : y : _) -> (x, y)) ll
  return (l1, l2)

f (l1, l2) =
  let m = foldl (\m e ->
          case M.lookup e m of
          Just k -> M.insert e (k + 1) m
          Nothing -> M.insert e 1 m) M.empty l2
      s = map (\x -> (x, M.findWithDefault 0 x m)) l1 in
  foldl (\s (x, y) -> s + (x * y)) 0 s

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

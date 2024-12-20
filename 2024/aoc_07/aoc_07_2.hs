import System.Environment (getArgs)
import Data.List.Split

parse :: FilePath -> IO [(Int, [Int])]
parse path = do
  c <- readFile path
  let l = lines c
      ll = map ((\[a, b] -> (read a, map read $ words b)) . splitOn ": ") l
  return ll

intcat a b = read $ show a ++ show b

treeSearch obj [a] = obj == a
treeSearch obj (a : b : l) =
  treeSearch obj ((a + b) : l) || treeSearch obj ((a * b) : l) || treeSearch obj ((a `intcat` b) : l)

f ll =
  sum $ map fst $ filter (uncurry treeSearch) ll

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

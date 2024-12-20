import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Data.Map (Map)
import qualified Data.Map as M

parse path = do
  c <- readFile path
  let a : _ : l = lines c
      t = splitOn ", " a
  return (t, l)

f (towels, styles) =
  sum $ map (snd . search M.empty towels) styles
 where
  splits l = map (`splitAt` l) [1..length l]
  search mem towels [] = (mem, 1)
  search mem towels style =
    case mem M.!? style of
    Just e -> (mem, e)
    Nothing ->
      foldr (\(_, e) (mem, x) ->
        let (mem', x') = search mem towels e in
        (M.insert e x' mem', x + x')) (mem, 0) (filter ((`elem` towels) . fst) (splits style))

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

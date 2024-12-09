import System.Environment (getArgs)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array.IArray
import Data.List (tails)

parse :: FilePath -> IO (Array (Int, Int) Char)
parse path = do
  c <- readFile path
  let (a : l) = lines c
      dx = length a
      dy = length (a : l)
  return $ listArray ((1, 1), (dy, dx)) $ concat (a : l)

antennas :: Array (Int, Int) Char -> Map Char [(Int, Int)]
antennas a = Map.delete '.' $ foldr (\i m -> Map.insertWith (++) (a ! i) [i] m) Map.empty (indices a)

antinodes a =
  let ((ly, lx), (uy, ux)) = bounds a 
      at = antennas a
      an = concatMap (\c -> [(x, y) | (x : ys) <- tails (at Map.! c), y <- ys]) (Map.keys at) in
  Set.filter (\(y, x) -> (y >= ly) && (y <= uy) && (x >= lx) && (x <= ux)) $
  foldr (\((y1, x1), (y2, x2)) s ->
    let (oy, ox) = (y2 - y1, x2 - x1) in
    Set.insert (y1 - oy, x1 - ox) (Set.insert (y2 + oy, x2 + ox) s)) Set.empty an

f a = 
  length $ antinodes a

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

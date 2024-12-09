import System.Environment (getArgs)
import Data.Array.IArray
import Data.Foldable (find)
import Data.List.Extra (firstJust)
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace (trace)

parse :: FilePath -> IO (Array (Int, Int) Char)
parse path = do
  c <- readFile path
  let (h : l) = lines c
      dx = length h
      dy = length (h : l)
      a = listArray ((1, 1), (dx, dy)) (concat (h : l))
  return a

data Dir = U | D | L | R deriving (Show, Eq)

data MapState a = Continue a | End | Loop deriving (Show, Eq)

guyDir c =
  case c of
  '^' -> Just U
  '>' -> Just R
  'v' -> Just D
  '<' -> Just L
  _ -> Nothing

dirOfs d =
  case d of
  U -> (-1,  0)
  R -> ( 0,  1)
  D -> ( 1,  0)
  L -> ( 0, -1)

guy d =
  case d of
  U -> '^'
  R -> '>'
  D -> 'v'
  L -> '<'

crumbs d =
  case d of
  U -> 'U'
  R -> 'R'
  D -> 'D'
  L -> 'L'

turnGuy d =
  case d of
  U -> R
  R -> D
  D -> L
  L -> U
         
findGuy :: Array (Int, Int) Char -> Maybe ((Int, Int), Dir)
findGuy a =
  firstJust (\(i, e) ->
    do d <- guyDir e
       return (i, d)) (assocs a)

stepGuy :: Array (Int, Int) Char -> Int -> Int -> Dir -> MapState (Array (Int, Int) Char, Int, Int, Dir)
stepGuy a y x d =
  let ((_, _), (dx, dy)) = bounds a
      (oy, ox) = dirOfs d
      (ny, nx) = (y + oy, x + ox) in
      if ny >= 1 && ny <= dy && nx >= 1 && nx <= dx then
        if a ! (ny, nx) == crumbs d then Loop else
        if a ! (ny, nx) /= '#' then
          Continue (a // [((ny, nx), crumbs d)], ny, nx, d)
        else stepGuy a y x (turnGuy d)
      else End

g a y x d =
  --trace (show (y, x)) $
  case stepGuy a y x d of
  Continue (a, y', x', d') -> g a y' x' d'
  e -> e

f a =
  length $ filter (== Loop) $ mapMaybe (\i ->
    case a ! i of
    '.' ->
      let ((y, x), d) = fromJust $ findGuy a in
      Just (g (a // [(i, '#')]) y x d)
    _ -> Nothing) (indices a)

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

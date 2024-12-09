import System.Environment (getArgs)
import Data.Array.IArray
import Data.Foldable (find)
import Data.List.Extra (firstJust)
import Data.Maybe (fromJust)

parse :: FilePath -> IO (Array (Int, Int) Char)
parse path = do
  c <- readFile path
  let (h : l) = lines c
      dx = length h
      dy = length (h : l)
      a = listArray ((1, 1), (dx, dy)) (concat (h : l))
  return a

data Dir = U | D | L | R deriving Show

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

turnGuy d =
  case d of
  U -> '>'
  R -> 'v'
  D -> '<'
  L -> '^'
         
findGuy :: Array (Int, Int) Char -> Maybe ((Int, Int), Dir)
findGuy a =
  firstJust (\(i, e) ->
    do d <- guyDir e
       return (i, d)) (assocs a)

stepGuy a =
  let ((_, _), (dx, dy)) = bounds a
      ((y, x), d) = fromJust $ findGuy a
      (oy, ox) = dirOfs d
      (ny, nx) = (y + oy, x + ox) in
      if (ny, nx) `elem` indices a then
        if a ! (ny, nx) /= '#' then Left (a // [((ny, nx), guy d), ((y, x), 'X')])
        else Left (a // [((y, x), turnGuy d)])
      else Right (a // [((y, x), 'X')])

f a =
  case stepGuy a of
  Left a -> f a
  Right a -> length $ filter (== 'X') (elems a)

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Data.Array (Array)
import Data.Array.Base (amap, bounds, (!), (//))
import Data.Array.IO (MArray (newArray), freeze, newListArray, writeArray)
import Data.Array.IO.Internals (IOArray (IOArray))
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import GHC.Base (maxInt)
import System.Environment (getArgs)

parse path = do
  c <- readFile path
  let l = lines c
      ll = map (readCouple . splitOn ",") l
  return ll
 where
  readCouple [a, b] = (read a, read b)

neighbors = [(0, 1), (0, -1), (1, 0), (-1, 0)]

applyOffs (y, x) (oy, ox) = (y + oy, x + ox)

astar arr start exit = loop exit score q
 where
  q = [start]
  score = amap (,maxInt) arr // [(start, (False, 0))]
  dist (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)
  minNode exit score = minNode' exit score Nothing
   where
    minNode' _ _ min [] = min
    minNode' exit score (Just (pm, sm)) (a : lq) =
      minNode' exit score (Just (if sa' < sm' then (a, sa) else (pm, sm))) lq
     where
      sa = snd (score ! a)
      sa' = dist a exit + sa
      sm' = dist pm exit + sm
    minNode' exit (score :: Array (Int, Int) (Bool, Int)) Nothing (a : lq) =
      minNode' exit score (Just (a, snd (score ! a))) lq
  tryRemove _ [] = []
  tryRemove e (e' : l) = if e' == e then l else e' : tryRemove e l
  tryAdd e [] = [e]
  tryAdd e (e' : l) = e' : if e' == e then l else tryAdd e l
  inBounds ((ly, lx), (uy, ux)) (y, x) = y >= ly && y <= uy && x >= lx && x <= ux
  loop exit score q =
    case minNode exit score q of
      Nothing -> Nothing
      Just (u, su) ->
        if u == exit
          then Just su
          else loop exit score' q''
       where
        q' = tryRemove u q
        n = mapMaybe (notWall score) $ filter (inBounds bnd) $ map (applyOffs u) neighbors
        (score', q'') = foldr (updateNeighbor su) (score, q') n
   where
    bnd = bounds score
    updateNeighbor su (v, sv) (score :: Array (Int, Int) (Bool, Int), q) =
      if su + 1 < sv
        then (score // [(v, (False, su + 1))], tryAdd v q)
        else (score, q)
    notWall (score :: Array (Int, Int) (Bool, Int)) p =
      case score ! p of
        (False, s) -> Just (p, s)
        _ -> Nothing

f l = do
  let (ll, ll') = splitAt 1024 l
  marr <- newListArray ((1, 1), (71, 71)) (repeat False)
  mapM_ (placeData marr) ll
  searchObstruct marr ll'
 where
  searchObstruct _ [] = error "could not find obstruction"
  searchObstruct marr (a : ll) = do
    placeData marr a
    arr <- freeze marr :: IO (Array (Int, Int) Bool)
    case astar arr (1, 1) (71, 71) of
      Just _ -> searchObstruct marr ll
      Nothing -> return a
  placeData :: IOArray (Int, Int) Bool -> (Int, Int) -> IO ()
  placeData marr (x, y) = writeArray marr (y + 1, x + 1) True

main = do
  (a : _) <- getArgs
  l <- parse a
  o <- f l
  print o

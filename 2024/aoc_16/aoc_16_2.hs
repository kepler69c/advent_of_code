{-# LANGUAGE TupleSections #-}

import Data.Array
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Base (maxInt)
import System.Environment (getArgs)

data Dir = U | D | L | R deriving (Show, Ord, Eq)

neighbors = [(0, 1), (0, -1), (1, 0), (-1, 0)]
neighborsDir = zip neighbors [R, L, D, U]

applyOffs (y, x) (oy, ox) = (y + oy, x + ox)

validDir d d' =
  case d of
    U -> d' /= D
    D -> d' /= U
    L -> d' /= R
    R -> d' /= L

createGraph s p d =
  fst $ createGraph' s s p d
 where
  createGraph' rs s p d =
    if not $ S.member (p, d) rs
      then ([], rs)
      else
        let rs' = S.delete (p, d) rs
            nn =
              filter (\(p', d') -> S.member (p', d') s && validDir d d') $
                map (first $ applyOffs p) neighborsDir
         in if null nn
              then ([], rs)
              else
                foldr
                  ( \(p', d') (l, rs) ->
                      if S.member (p', d') rs
                        then
                          let (l', rs') = createGraph' rs s p' d'
                           in (l ++ l', rs')
                        else (l, rs)
                  )
                  ([(p, d, nn)], rs')
                  nn

parse path = do
  c <- readFile path
  let (a : l) = lines c
      dy = length (a : l)
      dx = length a
      arr = listArray ((1, 1), (dy, dx)) (concat (a : l))
      nodes = S.fromList $ map fst $ filter ((/= '#') . snd) $ assocs arr
      nodesDir = foldr ((S.union . (\(s, d) -> S.map (,d) s)) . (nodes,)) S.empty [R, L, D, U]
      graph = createGraph nodesDir (dy - 1, 2) L
  return (graph, ((dy - 1, 2), L), (2, dx - 1))

dijkstra g pe =
  let e = fromJust $ findGraph pe g
      d = M.insert e 0 $ M.fromList $ map (,maxInt) g
   in loop g d M.empty
 where
  findGraph ((py, px), d) = find (\((py', px'), d', _) -> px == px' && py == py' && d == d')
  minNode d =
    foldr
      ( \e ec' ->
          let c = fromJust $ M.lookup e d
           in case ec' of
                Just (e', cmin) -> Just $ if c < cmin then (e, c) else (e', cmin)
                Nothing -> Just (e, c)
      )
      Nothing
  tryRemove _ [] = []
  tryRemove e (e' : l) = if e' == e then l else e' : tryRemove e l
  loop q d path =
    case minNode d q of
      Nothing -> (d, path)
      Just (u, c) -> loop q' d' path'
       where
        q' = tryRemove u q
        ((py, px), dir, n) = u
        (d', path') =
          foldr
            ( \v' (d, path) ->
                case findGraph v' q' of
                  Nothing -> (d, path)
                  Just v ->
                    if alt == c'
                      then (d, M.insertWith (++) v [u] path)
                      else
                        if alt < c' then (M.insert v alt d, M.insert v [u] path) else (d, path)
                   where
                    c' = d M.! v
                    ((py', px'), dir', _) = v
                    alt = c + if dir == dir' then 1 else 1001
            )
            (d, path)
            n

reconstruct n paths =
  case paths M.!? n of
    Just l -> foldr (\n' s -> S.union s (reconstruct n' paths)) (S.singleton (py, px)) l
    Nothing -> S.singleton (py, px)
 where
  ((py, px), _, _) = n

f (g, pe, ps) =
  length $ reconstruct (snd $ minimum pel) paths
 where
  (d, paths) = dijkstra g pe
  pel = map (\n -> (d M.! n, n)) $ filter (\((py, px), _, _) -> (py, px) == ps) $ M.keys paths

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

-- this fraud works

{-# LANGUAGE TupleSections #-}

import Data.Array
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import System.Environment (getArgs)

data Dir = U | D | L | R deriving (Show, Ord, Eq)

newtype Node = Node (Maybe Int, ((Int, Int), Dir, [((Int, Int), Dir)]))
  deriving (Eq, Show)

instance Ord Node where
  (Node (c1, _)) <= (Node (c2, _)) =
    case (c1, c2) of
      (Just c1', Just c2') -> c1' <= c2'
      (Nothing, Just _) -> False
      _ -> True

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

findPosDir s (p', d') = find (\(Node (c, (p, d, n))) -> p == p' && d == d') s

dijkstra g pe ps =
  let s = S.fromList $ map (\e -> Node (Nothing, e)) g
      Node (c, (p, d, n)) = fromJust $ findPosDir s pe
      s' =
        S.insert (Node (Just 0, (p, d, n))) $
          S.delete (Node (c, (p, d, n))) s
   in loop s'
 where
  updateCost s (Node (Just c1, (p1, d1, n1))) (Node (c2, (p2, d2, n2))) =
    let w = if d1 == d2 then 1 else 1001
     in if Node (c2, (p2, d2, n2)) > Node (Just (c1 + w), (p1, d1, n1))
          then
            let s' = S.delete (Node (c2, (p2, d2, n2))) s
             in S.insert (Node (Just (c1 + w), (p2, d2, n2))) s'
          else s
  loop s =
    case S.minView s of
      Nothing -> error "exit not found"
      Just (Node (c, (p, d, n)), s') ->
        if p == ps
          then fromJust c
          else
            let s'' =
                  foldr
                    ( \(p', d') s ->
                        case findPosDir s (p', d') of
                          Nothing -> s
                          Just n2 -> updateCost s (Node (c, (p, d, n))) n2
                    )
                    s'
                    n
             in loop s''

f (g, pe, ps) = dijkstra g pe ps

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)
import Data.Array.IArray
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (find)

parse :: FilePath -> IO (Array (Int, Int) Char)
parse path = do
  c <- readFile path
  let (h : l) = lines c
      dx = length h
      dy = length (h : l)
      a = listArray ((1, 1), (dx, dy)) (concat (h : l))
  return a

neighbors4 = [(0, 1), (1, 0), (0, -1), (-1, 0)]

inBounds a (y, x) =
  let ((ly, lx), (uy, ux)) = bounds a in
  (y >= ly) && (y <= uy) && (x >= lx) && (x <= ux)

dfs :: Array (Int, Int) (Bool, Char) -> Char -> (Int, Int) -> ([(Int, Int)], Array (Int, Int) (Bool, Char))
dfs a c (y, x) =
  let n = filter (inBounds a) $ map (bimap (+ y) (+ x)) neighbors4 in
  foldr (\i (s, a) ->
    if (a ! i) == (True, c) then
      let (s', a') = dfs (a // [(i, (False, c))]) c i in
      (s ++ s', a')
    else (s, a)) ([(y, x)], a) n

collectFields a =
  let ba = amap (True,) a in
  collectF ba
  where
  collectF ba =
    case find (fst . snd) (assocs ba) of
    Just (p, (_, c)) ->
      let (f, ba') = dfs (ba // [(p, (False, c))]) c p in
      (f, c) : collectF ba'
    Nothing -> []

neighbors8 = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

cornersF = [
  \case
  [ _    , False, _    ,
    False,        False,
    _    , False, _      ] -> 4
  _ -> 0,
  \case
  [ _    , True , _    ,
    False,        False,
    _    , False, _      ] -> 2
  _ -> 0,
  \case
  [ _    , False, _    ,
    False,        True ,
    _    , False, _      ] -> 2
  _ -> 0,
  \case
  [ _    , False, _    ,
    False,        False,
    _    , True , _      ] -> 2
  _ -> 0,
  \case
  [ _    , False, _    ,
    True ,        False,
    _    , False, _      ] -> 2
  _ -> 0,
  \case
  [ _    , True , _    ,
    True ,        False,
    _    , False, _      ] -> 1
  _ -> 0,
  \case
  [ _    , True , _    ,
    False,        True ,
    _    , False, _      ] -> 1
  _ -> 0,
  \case
  [ _    , False, _    ,
    False,        True ,
    _    , True , _      ] -> 1
  _ -> 0,
  \case
  [ _    , False, _    ,
    True ,        False,
    _    , True , _      ] -> 1
  _ -> 0,
  \case
  [ False, True , _    ,
    True ,        _    ,
    _    , _    , _      ] -> 1
  _ -> 0,
  \case
  [ _    , True , False,
    _    ,        True ,
    _    , _    , _      ] -> 1
  _ -> 0,
  \case
  [ _    , _    , _    ,
    _    ,        True ,
    _    , True , False  ] -> 1
  _ -> 0,
  \case
  [ _    , _    , _    ,
    True ,        _    ,
    False, True , _      ] -> 1
  _ -> 0]

computeAP a (ps, c) =
  let area = length ps
      sides = sum $
              map ((\l -> sum $ map ($ l) cornersF) .
                   (\(y, x) -> map (\(oy, ox) ->
                     let i' = (oy + y, ox + x) in
                     (inBounds a i' && (a ! i') == c)) neighbors8)) ps in
      (area, sides)

f a =
  let (areas, sides) = unzip $ map (computeAP a) (collectFields a) in
  sum $ zipWith (*) areas sides

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

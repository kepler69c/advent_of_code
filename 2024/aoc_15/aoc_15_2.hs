import System.Environment (getArgs)
import Data.List.Split
import Data.Array
import Data.Array.IO
import Data.Array.IArray (amap)
import Data.List (find, intercalate)
import Data.Maybe (fromJust)
import Control.Monad (foldM)

data Pool = Wall | Box1 | Box2 | Empty deriving Eq
data Dir = U | D | L | R deriving Show

widen c =
  case c of
  '#' -> "##"
  'O' -> "[]"
  '@' -> "@ "
  '.' -> ".."

parsePool c =
  case c of
  '#' -> Wall
  '[' -> Box1
  ']' -> Box2
  _ -> Empty

parseDir c =
  case c of
  '^' -> U
  'v' -> D
  '<' -> L
  _ -> R

parse :: FilePath -> IO (IOArray (Int, Int) Pool, (Int, Int), [Dir])
parse path = do
  c <- readFile path
  let l = lines c
      [al, b] = splitOn [""] l
      (a : al')  = map (concatMap widen) al
      dy = length (a : al')
      dx = length a
      iarr = listArray ((1, 1), (dy, dx)) (concat (a : al'))
      pos = fst $ fromJust $ find ((== '@') . snd) (assocs iarr)
      iarr' = amap parsePool iarr
      dirs = map parseDir (concat b)
  marr <- thaw iarr'
  return (marr, pos, dirs)

offDir d =
  case d of
  U -> (-1, 0)
  D -> (1, 0)
  L -> (0, -1)
  R -> (0, 1)

newDir d (py, px) =
  let (oy, ox) = offDir d in
  (py + oy, px + ox)

sideBoxPos c (py, px) =
  case c of
  Box1 -> (py, px + 1)
  Box2 -> (py, px - 1)

checkCollision :: IOArray (Int, Int) Pool -> (Int, Int) -> Dir -> Pool -> IO Bool
checkCollision a p d Empty = return True
checkCollision a p d Wall = return False
checkCollision a p d c = do
  let sp = sideBoxPos c p
      p' = newDir d p
      sp' = newDir d sp
  e1 <- readArray a p'
  e2 <- readArray a sp'
  if p' == sp || c == e1
    then checkCollision a sp' d e2
    else do
      r1 <- checkCollision a p' d e1
      r2 <- checkCollision a sp' d e2
      return (r1 && r2)

actualBoxMove :: IOArray (Int, Int) Pool -> (Int, Int) -> Dir -> Pool -> IO (IOArray (Int, Int) Pool)
actualBoxMove a p d Empty = return a
actualBoxMove a p d Wall = error "Wall during the push"
actualBoxMove a p d c = do
  let sp = sideBoxPos c p
      p' = newDir d p
      sp' = newDir d sp
  e2 <- readArray a sp'
  e1 <- readArray a p'
  writeArray a p Empty
  writeArray a sp Empty
  if p' == sp || c == e1
    then do
      a <- actualBoxMove a sp' d e2
      writeArray a p' c
      writeArray a sp' (if c == Box1 then Box2 else Box1)
      return a
    else do
      a <- actualBoxMove a p' d e1
      a <- actualBoxMove a sp' d e2
      writeArray a p' c
      writeArray a sp' (if c == Box1 then Box2 else Box1)
      return a

move :: IOArray (Int, Int) Pool -> (Int, Int) -> Dir -> IO (IOArray (Int, Int) Pool, (Int, Int))
move a p d = do
  let p' = newDir d p
  e <- readArray a p'
  case e of
    Wall -> return (a, p)
    Empty -> return (a, p')
    _ -> do
      ok <- checkCollision a p' d e
      if ok
        then do
          a <- actualBoxMove a p' d e
          return (a, p')
        else return (a, p)

instance Show Pool where
  show p =
    case p of
    Empty -> "."
    Box1 -> "["
    Box2 -> "]"
    Wall -> "#"

display :: IOArray (Int, Int) Pool -> (Int, Int) -> IO ()
display a p = do
  a' <- mapArray (head . show) a
  writeArray a' p '@'
  e <- getElems a'
  ((_, _), (by, bx)) <- getBounds a'
  putStrLn $ intercalate "\n" $ chunksOf bx e

f (a, p, ds) = do
  (a', p') <- foldM (\(a, p) d -> do move a p d) (a, p) ds
  --display a' p'
  asc <- getAssocs a'
  return $ sum $ map ((\(y, x) -> 100 * (y - 1) + (x - 1)) . fst) $ filter ((== Box1) . snd) asc

main = do
  (a : _) <- getArgs
  l <- parse a
  r <- f l
  print r

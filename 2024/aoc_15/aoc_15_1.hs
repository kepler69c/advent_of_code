import System.Environment (getArgs)
import Data.List.Split
import Data.Array
import Data.Array.IO
import Data.Array.IArray (amap)
import Data.List (find, intercalate)
import Data.Maybe (fromJust)
import Control.Monad.Loops (takeWhileM)
import Control.Monad (foldM)

data Pool = Wall | Box | Empty deriving Eq
data Dir = U | D | L | R

parsePool c =
  case c of
  '#' -> Wall
  'O' -> Box
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
      [a : al, b] = splitOn [""] l
      dy = length (a : al)
      dx = length a
      iarr = listArray ((1, 1), (dy, dx)) (concat (a : al))
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

move :: IOArray (Int, Int) Pool -> (Int, Int) -> Dir -> IO (IOArray (Int, Int) Pool, (Int, Int))
move a p d = do
  let p' = newDir d p
  e <- readArray a p'
  case e of
    Wall -> return (a, p)
    Empty -> return (a, p')
    Box -> do
      movl <- takeWhileM (\p -> do
        e <- readArray a p
        return (e == Box)) (iterate (newDir d) p')
      let p'' = newDir d (last movl)
      e <- readArray a p''
      case e of
        Wall -> return (a, p)
        _ -> do
          writeArray a p' Empty
          writeArray a p'' Box
          return (a, p')

instance Show Pool where
  show p =
    case p of
    Empty -> "."
    Box -> "O"
    Wall -> "#"

display :: IOArray (Int, Int) Pool -> (Int, Int) -> IO ()
display a p = do
  a' <- mapArray (head . show) a
  writeArray a' p '@'
  e <- getElems a'
  ((_, _), (by, bx)) <- getBounds a'
  putStrLn $ intercalate "\n" $ chunksOf bx e

f (a, p, ds) = do
  asc <- getAssocs a
  (a', p') <- foldM (\(a, p) d -> move a p d) (a, p) ds
  --display a' p'
  asc <- getAssocs a'
  return $ sum $ map ((\(y, x) -> 100 * (y - 1) + (x - 1)) . fst) $ filter ((== Box) . snd) asc

main = do
  (a : _) <- getArgs
  l <- parse a
  r <- f l
  print r

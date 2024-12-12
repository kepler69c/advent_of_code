import System.Environment (getArgs)
import qualified Data.HashTable.IO as H
import Control.Monad (foldM)

type HashTable k v = H.BasicHashTable k v

parse path = do
  c <- readFile path
  let (a : l) = lines c
  return (map read (words a))

blink :: HashTable (Integer, Integer) Integer -> Integer -> Integer ->
         IO (HashTable (Integer, Integer) Integer, Integer)
blink mem i n =
  if i == 0 then return (mem, 1) else do
  e <- H.lookup mem (n, i)
  case e of
    Just x -> return (mem, x)
    Nothing -> do
      let str = show n
          len = length str
      (mem, r) <- if n == 0 then blink mem (i - 1) 1
                  else if even len then do
                    let (a, b) = splitAt (len `div` 2) str
                    (mem, r1) <- blink mem (i - 1) (read a)
                    (mem, r2) <- blink mem (i - 1) (read b)
                    return (mem, r1 + r2)
                  else blink mem (i - 1) (2024 * n)
      H.insert mem (n, i) r
      return (mem, r)

f l = do
  mem <- H.new
  (_, n) <- foldM (\(mem, s) n -> do
              (mem, n) <- blink mem 75 n
              return (mem, s + n)) (mem, 0) l
  return n

main = do
  (a : _) <- getArgs
  l <- parse a
  r <- f l
  print r

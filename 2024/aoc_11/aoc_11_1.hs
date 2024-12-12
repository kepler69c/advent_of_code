import System.Environment (getArgs)

parse :: FilePath -> IO [Integer]
parse path = do
  c <- readFile path
  let (a : l) = lines c
  return (map read (words a))

blink :: [Integer] -> [Integer]
blink [] = []
blink (a : l) =
  let str = show a 
      len = length str in
  if a == 0 then 1 : blink l else
  if even len then
    let (a, b) = splitAt (len `div` 2) str in
    [read a, read b] ++ blink l
  else 2024 * a : blink l

f l = length $ iterate blink l !! 25

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

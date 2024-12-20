import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Word (Word64)
import Data.Bits ((.>>.), (.<<.), (.&.), xor)

parse path = do
  c <- readFile path
  let [_, _, _, _, pl] = lines c
      prog = map read (splitOn "," $ words pl !! 1) :: [Word64]
  return prog

-- this function is based on the given input
exec ra = 5 `xor` (ra .>>. fromIntegral (3 `xor` (ra .&. 7))) `xor` (3 `xor` (ra .&. 7))

tree [] ra = [ra]
tree (a : l) ra =
  let ras = map (\x -> (x, exec ((ra .<<. 3) + x))) [0..7] in
  foldr (\(i, x) lra ->
    if x .&. 7 == a
      then lra ++ tree l ((ra .<<. 3) + i)
      else lra) [] ras

f prog = minimum $ tree (reverse prog) 0

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

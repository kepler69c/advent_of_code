import System.Environment (getArgs)
import Text.Regex.TDFA

parse path = do readFile path

f :: String -> Int
f l =
  let mulRegex = "mul\\(([0-9]+),([0-9]+)\\)" in
  case l =~ mulRegex :: (String, String, String, [String]) of
  (_, _, n, [a, b]) -> read a * read b + f n
  _ -> 0

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

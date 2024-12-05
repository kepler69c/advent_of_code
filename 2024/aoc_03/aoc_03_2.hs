import System.Environment (getArgs)
import Text.Regex.TDFA

parse path = do readFile path

f :: String -> Bool -> Int
f l t =
  let mulRegex = "mul\\(([0-9]+),([0-9]+)\\)|(do(n't)?\\(\\))" in
  case l =~ mulRegex :: (String, String, String, [String]) of
  (_, _, n, [_, _, "do()", _]) -> f n True
  (_, _, n, [_, _, "don't()", _]) -> f n False
  (_, _, n, [a, b, _, _]) -> (if t then read a * read b else 0) + f n t
  _ -> 0

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l True)

import System.Environment (getArgs)
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable (find)

parse path = do
  c <- readFile path
  let [la, lb] = splitOn [""] (lines c)
      la' = M.fromListWith S.union $ map ((\[a, b] -> (a, S.singleton b)) . map read . splitOn "|") la
      lb' = map (map read . splitOn ",") lb
  return (la', lb')

validEntry m l =
  validEntry' m (reverse l)
  where
  validEntry' _ [] = True
  validEntry' m (a : l) =
    (case m M.!? a of
     Just ml -> all (`notElem` ml) l
     Nothing -> True) && validEntry' m l

replace _ _ [] = []
replace a b (x:l) =
  (if x == a then b else x) : replace a b l

fixEntry m l =
  let l' = reverse l
      l'' = fixEntry' m l' in
  if l' == l'' then reverse l' else fixEntry m (reverse l'')
  where
  fixEntry' _ [] = []
  fixEntry' m (a : l) =
    case m M.!? a of
    Just ml -> (case find (`elem` ml) l of
                Just e -> e : fixEntry' m (replace e a l)
                Nothing -> a : fixEntry' m l)
    Nothing -> a : fixEntry' m l

f (la, lb) =
  let lb' = map (fixEntry la) (filter (not . validEntry la) lb) in
  sum $ map (\l -> l !! (length l `div` 2)) lb'

main = do
  (a : _) <- getArgs
  l <- parse a
  print (f l)

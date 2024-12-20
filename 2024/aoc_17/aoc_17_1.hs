import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Word (Word64)
import Data.Bits ((.>>.), (.&.), xor)
import Data.List (intercalate)

data State = State
  { rA :: Word64
  , rB :: Word64
  , rC :: Word64
  , pC :: Word64 } deriving Show

parse path = do
  c <- readFile path
  let [ral, rbl, rcl, _, pl] = lines c
      state = State { rA = read (words ral !! 2)
                    , rB = read (words rbl !! 2)
                    , rC = read (words rcl !! 2)
                    , pC = 0 }
      prog = map read (splitOn "," $ words pl !! 1) :: [Word64]
  return (prog, state)

getArg arg state | arg == 4 = rA state
                 | arg == 5 = rB state
                 | arg == 6 = rC state
                 | otherwise = arg

step op arg state =
  case op of
  0 -> -- adv
    (state { rA = rA state .>>. fromIntegral (getArg arg state)
           , pC = 2 + pC state }, [])
  1 -> -- bxl
    (state { rB = rB state `xor` arg
           , pC = 2 + pC state }, [])
  2 -> -- bst
    (state { rB = getArg arg state .&. 0x7
           , pC = 2 + pC state }, [])
  3 -> -- jnz
    (if rA state == 0
       then state { pC = 2 + pC state }
       else state { pC = arg }, [])
  4 -> -- bxc
    (state { rB = rB state `xor` rC state
           , pC = 2 + pC state }, [])
  5 -> -- out
    (state { pC = 2 + pC state }, [ getArg arg state .&. 0x7 ])
  6 -> -- bdv
    (state { rB = rA state .>>. fromIntegral (getArg arg state)
           , pC = 2 + pC state }, [])
  7 -> -- cdv
    (state { rC = rA state .>>. fromIntegral (getArg arg state)
           , pC = 2 + pC state }, [])

loop prog state =
  if pC state >= fromIntegral (length prog) then [] else
  let (op, arg) = (prog !! fromIntegral (pC state), prog !! fromIntegral (pC state + 1))
      (state', out) = step op arg state in
  out ++ loop prog state'

f (prog, state) = intercalate "," $ map show $ loop prog state

main = do
  (a : _) <- getArgs
  l <- parse a
  putStrLn (f l)

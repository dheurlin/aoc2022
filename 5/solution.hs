import Data.List ( transpose )
import Data.Char ( isDigit, isAlphaNum )
import Data.Maybe ( mapMaybe )
import Data.Map ( Map )
import qualified Data.Map as Map

type Stack a = [a]

type Cargo a = Map Int (Stack a)

parseCargo :: String -> Cargo Char
parseCargo = Map.fromList
  . zip [1..]
  . map (filter (/= ' '))
  . filter isStack
  . transpose
  . takeWhile (not . isNumbers)
  . lines
  where
    isNumbers  = all (\c -> isDigit c || c == ' ')
    isStack cs = all (\c -> isAlphaNum c || c == ' ') cs && any (/= ' ') cs

data Move = Move { num :: Int, src :: Int, dst :: Int }
  deriving ( Eq, Show )

parseMove :: String -> Move
parseMove s =
  let first     = takeWhile (/= 'f') s
      second    = takeWhile (/= 't') $ drop (length first) s
      third     = drop (length first + length second) s
      getNumber = read . filter isDigit
   in Move (getNumber first) (getNumber second) (getNumber third)

parseMoves :: String -> [Move]
parseMoves = map parseMove . tail . dropWhile (/= "") . lines

-- Performs one step of the procedure. If rev == True, will move the crates in
-- reverse order (star 1) and if False, will preserve their order (star 2)
step :: Bool -> Cargo a -> Move -> Cargo a
step rev cargo (Move num src dst) = Map.insert src src' $ Map.insert dst dst' cargo
  where
    op       = if rev then reverse else id
    srcStack = cargo Map.! src
    dstStack = cargo Map.! dst
    taken    = op $ take num srcStack
    src'     = drop num srcStack
    dst'     = taken <> dstStack

solve :: Bool -> String -> String
solve b input = getMessage $ solve b c ms
  where
    ms      = parseMoves input
    c       = parseCargo input
    solve b = foldl (step b)

star1 = solve True
star2 = solve False

getMessage :: Cargo Char -> String
getMessage = map (head . snd) . Map.toAscList

main = do
  input <- readFile "input.txt"
  print $ "Star 1: " <> star1 input
  print $ "Star 2: " <> star2 input

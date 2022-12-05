import Data.Char ( ord )
import Data.Set  ( Set )
import qualified Data.Set as Set

priority :: Char -> Int
priority = priority' . ord
  where
    priority' n | n >= 97 = n - 96
    priority' n           = n - 38

commonElem :: [Char] -> [Char] -> Char
commonElem [] _ = error "No common elements"
commonElem (x:xs) ys
  | x `elem` ys = x
  | otherwise   = commonElem xs ys

splitBag :: [Char] -> ([Char], [Char])
splitBag xs = case length xs of
  n | even n -> splitAt (n `div` 2) xs
  _          -> error "Uneven number of items in bag!"

type Group = ([Char], [Char], [Char])

splitGroups :: [[Char]] -> [Group]
splitGroups (a:b:c:xs) = (a, b, c) : splitGroups xs
splitGroups []         = []
splitGroups _          = error "Indivisible number of groups!"

common3 :: Group -> Char
common3 (a,b,c) = Set.elemAt 0 sect
  where
    sect = Set.intersection (Set.fromList a) $ Set.intersection (Set.fromList b) (Set.fromList c)

star1 :: String -> Int
star1 = sum
  . map (priority . uncurry commonElem . splitBag)
  . filter (/= "") . lines

star2 :: String -> Int
star2 = sum
  . map (priority . common3) . splitGroups
  . filter (/= "") . lines

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Star 1: " <> show (star1 input)
  putStrLn $ "Star 2: " <> show (star2 input)

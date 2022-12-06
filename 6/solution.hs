import Data.List ( nub )

star1 :: String -> Int
star1 (a : b : c : d : xs)
  | nub [a, b, c, d] == [a, b, c, d] = 4
star1 xs = 1 + star1 (tail xs)

star2 :: String -> Int
star2 xs = case splitAt 14 xs of
 (xs, ys) | nub xs == xs -> 14
          | otherwise    -> 1 + star2 (tail xs <> ys)

main = do
  input <- readFile "input.txt"
  putStrLn $ "Star 1: " <> show (star1 input)
  putStrLn $ "Star 2: " <> show (star2 input)

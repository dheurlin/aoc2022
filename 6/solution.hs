import Data.List ( nub )

solve :: Int -> String -> Int
solve prefixLen str = case splitAt prefixLen str of
 (xs, ys) | nub xs == xs -> prefixLen
          | otherwise    -> 1 + solve prefixLen (tail xs <> ys)

star1 = solve 4
star2 = solve 14

main = do
  input <- readFile "input.txt"
  putStrLn $ "Star 1: " <> show (star1 input)
  putStrLn $ "Star 2: " <> show (star2 input)

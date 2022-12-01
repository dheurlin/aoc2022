import Data.List ( sort )

-- NOTE: this implementation does not handle edge cases well, but it's good
-- enoguh for this task.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = foldr step []
  where
    step curr []
      | curr == x = []
      | otherwise = [[curr]]
    step curr (hd:rst)
      | curr == x = [] : hd : rst
      | otherwise = (curr : hd) : rst

parse :: String -> [[Int]]
parse = (map . map) read . splitOn "" . lines

solve1 :: [[Int]] -> Int
solve1 = maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . reverse . sort . map sum

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  (print . ("Star 1: "<> )) . show . solve1 $ input
  (print . ("Star 2: "<> )) . show . solve2 $ input

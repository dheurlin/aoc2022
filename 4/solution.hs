
type Range = (Int, Int)

fullyOverlapping :: Range -> Range -> Bool
fullyOverlapping (x1, x2) (y1, y2) =
  (x1 <= y1 && x2 >= y2) ||
  (y1 <= x1 && y2 >= x2)

overlapping :: Range -> Range -> Bool
overlapping r1@(x1, x2) r2@(y1, y2) =
  x1 `within` r2 || x2 `within` r2 ||
  y1 `within` r1 || y2 `within` r1
  where
    x `within` (y1, y2) = x >= y1 && x <= y2

parseRange :: String -> Range
parseRange s = (read p1, read p2)
  where
    n = length p1
    p1 = takeWhile (/= '-') s
    p2 = drop (n +  1) s

parse :: String -> [(Range, Range)]
parse = map (parseRanges . splitComma). lines
  where
    parseRanges (a, b) = (parseRange a, parseRange b)
    splitComma s = (p1, drop n s)
      where
        p1 = takeWhile (/= ',') s
        n = length p1 + 1

star1 :: String -> Int
star1 = length . filter (uncurry fullyOverlapping) . parse

star2 :: String -> Int
star2 = length . filter (uncurry overlapping) . parse

main = do
  input <- readFile "input.txt"
  print $ "Star1: " <> show (star1 input)
  print $ "Star2: " <> show (star2 input)

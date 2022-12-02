data Move = Rock | Paper | Scissors
  deriving ( Eq, Show )

moveScore :: Move -> Int
moveScore Rock     = 1
moveScore Paper    = 2
moveScore Scissors = 3

parseMove :: String -> Move
parseMove "A" = Rock
parseMove "B" = Paper
parseMove "C" = Scissors
parseMove "X" = Rock
parseMove "Y" = Paper
parseMove "Z" = Scissors
parseMove m   = error $ "Invalid move: " <> m

data Outcome = Win | Lose | Draw
  deriving ( Eq, Show )

parseOutcome :: String -> Outcome
parseOutcome "X" = Lose
parseOutcome "Y" = Draw
parseOutcome "Z" = Win
parseOutcome o   = error $ "Invalid outcome: " <> o

outcomeScore :: Outcome -> Int
outcomeScore Win  = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

vs :: Move -> Move -> Outcome
Rock     `vs` Paper    = Lose
Rock     `vs` Scissors = Win
Paper    `vs` Rock     = Win
Paper    `vs` Scissors = Lose
Scissors `vs` Rock     = Lose
Scissors `vs` Paper    = Win
_        `vs` _        = Draw

chooseForOutcome :: Outcome -> Move -> Move
chooseForOutcome Win Rock      = Paper
chooseForOutcome Win Paper     = Scissors
chooseForOutcome Win Scissors  = Rock
chooseForOutcome Lose Paper    = Rock
chooseForOutcome Lose Scissors = Paper
chooseForOutcome Lose Rock     = Scissors
chooseForOutcome Draw m        = m

parse :: ([String] -> a) -> String -> [a]
parse parseRound = map (parseRound . words) . filter (/= "") .lines

--- Star 1 --------------------------------------------------------------------

data Round1 = Round1 { opponent :: Move, player :: Move }
  deriving ( Eq, Show )

parseRound1 :: [String] -> Round1
parseRound1 [op, me] = Round1 (parseMove op) (parseMove me)
parseRound1 r        = error $ "Invalid round: " <> unwords r

star1 :: String -> Int
star1 = sum . map score . parse parseRound1
  where
    score Round1 { opponent = op, player = me } =
      moveScore me + outcomeScore (me `vs` op)

--- Star 2 --------------------------------------------------------------------

data Round2 = Round2 { opponent2 :: Move, outcome :: Outcome }
  deriving ( Eq, Show )

parseRound2 :: [String] -> Round2
parseRound2 [op, oc] = Round2 (parseMove op) (parseOutcome oc)
parseRound2 r        = error $ "Invalid round: " <> unwords r

star2 :: String -> Int
star2 = sum . map score . parse parseRound2
  where
    score Round2 { opponent2 = op, outcome = oc } =
      let move = chooseForOutcome oc op
       in moveScore move + outcomeScore oc

-------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ "Star 1: " <> show (star1 input)
  print $ "Star 2: " <> show (star2 input)

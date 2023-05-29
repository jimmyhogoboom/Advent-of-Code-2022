module Day2.RPS where

import Data.Maybe
import Debug.Trace

fileName :: String
-- fileName = "app/Day2/testInput.txt"
fileName = "app/Day2/puzzleInput.txt"

part1 :: IO ()
part1 = do
  content <- readFile fileName
  -- let content = testData
  let points = parse1 content
  putStrLn "\nDay 2.1: Point from strategy: "
  print points

part2 :: IO ()
part2 = do
  content <- readFile fileName
  -- let content = testData
  let points = parse2 content
  putStrLn "\nDay 2.2: Point from strategy: "
  print points

testData = "A Y\nB X\nC Z"

-- Scoring:
-- The shape I select:
-- Rock = 1
-- Paper = 2
-- Scissors = 3
--
-- Outcome:
-- I lose = 0
-- Draw = 3
-- I win = 6
--
-- Mapping:
-- A -> Rock -> X
-- B -> Paper -> Y
-- C -> Scissors -> Z

-- NOTE: This could be done by creating recursive types and adding an instance of Ord
getScoreMove :: Char -> Char -> Int
getScoreMove x y =
  case y of
    -- I play X (Rock)
    'X' ->
      1 + case x of
        -- Opponent plays A (Rock)
        'A' -> 3 -- draw
        -- Opponent plays B (Paper)
        'B' -> 0 -- lose
        -- Opponent plays C (Scissors)
        'C' -> 6 -- win
        _ -> 0
    'Y' ->
      2 + case x of
        'A' -> 6
        'B' -> 3
        'C' -> 0
        _ -> 0
    'Z' ->
      3 + case x of
        'A' -> 0
        'B' -> 6
        'C' -> 3
        _ -> 0
    _ -> 0

getScoreEnd :: Char -> Char -> Int
getScoreEnd x y =
  case x of
    -- Opponent plays A (Rock)
    'A' -> case y of
      'X' -> 0 + 3 -- I must lose 0 (Scissors 3)
      'Y' -> 3 + 1 -- I must end in a draw 3 (Rock 1)
      'Z' -> 6 + 2 -- I must win! 6 (Paper 2)
      _ -> 0
    -- Opponent plays B (Paper)
    'B' -> case y of
      'X' -> 0 + 1 -- I must lose 0 (Rock 1)
      'Y' -> 3 + 2 -- I must end in a draw 3 (Paper 2)
      'Z' -> 6 + 3 -- I must win! 6 (Scissors 3)
      _ -> 0
    -- Opponent plays C (Scissors)
    'C' -> case y of
      'X' -> 0 + 2 -- I must lose 0 (Paper 2 )
      'Y' -> 3 + 3 -- I must end in a draw 3 (Scissors 3)
      'Z' -> 6 + 1 -- I must win! 6 (Rock 1)
      _ -> 0
    _ -> 0

parse1 :: String -> Int
parse1 = parseLines getScoreMove Nothing . lines

parse2 :: String -> Int
parse2 = parseLines getScoreEnd Nothing . lines

parseLines :: (Char -> Char -> Int) -> Maybe Int -> [String] -> Int
parseLines scoreFun currentScore lines =
  case lines of
    [] -> maybeScore
    (line : rest) ->
      if line == ""
        then maybeScore
        else case line of
          (x : ' ' : y : _) -> do
            parseLines scoreFun (Just (maybeScore + scoreFun x y)) rest
          _ -> maybeScore
  where
    maybeScore = fromMaybe 0 currentScore

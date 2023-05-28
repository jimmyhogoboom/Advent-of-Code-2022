module Day2.RPS where

import Data.Maybe
import Debug.Trace

fileName :: String
fileName = "app/Day2/puzzleInput.txt"

-- fileName = "app/Day2/testInput.txt"

part1 :: IO ()
part1 = do
  content <- readFile fileName
  -- let content = testData
  let points = parse1 content
  putStrLn "\nDay 2.1: Point from strategy: "
  print points

testData = "A Y\nB X\nB X\nC Y"

-- Mapping:
-- A -> Rock -> X
-- B -> Paper -> Y
-- C -> Scissors -> Z

-- Points:
-- Rock: 1
-- Paper: 2
-- Scissors: 3

-- NOTE: This could be done by creating recursive types and adding an instance of Ord
getScore :: Char -> Char -> Int
getScore x y =
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

parse1 :: String -> Int
parse1 = parseLines getScore Nothing . lines

-- parse :: String -> Int
-- parse = parseLines Nothing . lines

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

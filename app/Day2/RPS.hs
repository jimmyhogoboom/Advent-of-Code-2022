module Day2.RPS where

import Data.Maybe

fileName :: String
fileName = "app/Day2/puzzleInput.txt"

part1 :: IO ()
part1 = do
  -- content <- readFile fileName
  let content = testData
  let points = parse content
  putStrLn "\nDay 2.1: Point from strategy: "
  print $ points
  print $ getScore "A" "Z"

-- 1        2         3
-- A: Rock, B: Paper, C: Scissors
-- X: Rock, Y: Paper, Z: Scissors

testData = "A Y\nB X\nC Z"

-- TODO: get score from values: do a sort on the two?
getScore :: String -> String -> Maybe Int
getScore x y =
  Just
    ( case x of
        "A" ->
          1 + case y of
            "X" -> 3
            "Y" -> 0
            "Z" -> 6
            _ -> 0
        "B" ->
          2 + case y of
            "X" -> 6
            "Y" -> 3
            "Z" -> 0
            _ -> 0
        "C" ->
          3 + case y of
            "X" -> 0
            "Y" -> 6
            "Z" -> 3
            _ -> 0
        _ -> 0
    )

parse :: String -> Int
parse = parseLines Nothing . lines

-- TODO: map win/tie/loss to points and return
parseLines :: Maybe Int -> [String] -> Int
parseLines currentScore lines =
  case lines of
    [] -> maybeScore
    (line : rest) ->
      if line == ""
        then maybeScore
        else case break (== ' ') line of
          (x, y) ->
            parseLines (Just (maybeScore + fromMaybe 0 (getScore x y))) rest
          _ ->
            parseLines currentScore rest
  where
    maybeScore = fromMaybe 0 currentScore

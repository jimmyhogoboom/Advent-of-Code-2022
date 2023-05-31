module Day3.Ruck where

import Data.List

fileName :: String
-- fileName = "app/Day3/testInput.txt"
fileName = "app/Day3/puzzleInput.txt"

part1 :: IO ()
part1 = do
  -- content <- readFile fileName
  -- let content = testData
  -- let result = parse1 content
  putStrLn "\nDay 3.1:"
  -- print result
  let compartments = stringToCompartments 2 "vJrwpWtwJgWrhcsFMMfFFhFp"
   in putStrLn $ compartmentsIntersection (head compartments) (last compartments)

newtype Compartment = Compartment {items :: [Char]}

displayCompartment (Compartment c) = "Compartment " ++ show c

showCompartment (Compartment c) = show c

newtype Row = Row {compartments :: [Compartment]}

showRow (Row r) = "Row " ++ show (map showCompartment r)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first, rest) = splitAt n list

stringToCompartments :: Int -> String -> [Compartment]
stringToCompartments compartmentCount input =
  let compartmentLength = length input `div` compartmentCount
   in map Compartment $ splitEvery compartmentLength input

stringToRow :: Int -> String -> Row
stringToRow compartmentCount input =
  Row $ stringToCompartments compartmentCount input

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection [] _ = []
intersection _ [] = []
intersection as bs = filter (`elem` bs) as

compartmentsIntersection :: Compartment -> Compartment -> [Char]
compartmentsIntersection (Compartment a) (Compartment b) = intersection a b

-- parse1 :: String -> Int
-- parse1 = parseLines getScoreEnd Nothing . lines

-- parseLines :: (Char -> Char -> Int) -> Maybe Int -> [String] -> Int
-- parseLines scoreFun currentScore lines =
--   case lines of
--     [] -> maybeScore
--     (line : rest) ->
--       if line == ""
--         then maybeScore
--         else case line of
--           (x : ' ' : y : _) -> do
--             parseLines scoreFun (Just (maybeScore + scoreFun x y)) rest
--           _ -> maybeScore
--   where
--     maybeScore = fromMaybe 0 currentScore

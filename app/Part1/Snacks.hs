module Part1.Snacks where

import Data.List
import Data.Maybe (maybeToList)

type Snack = Int

newtype Elf = Elf {snacks :: [Snack]} deriving (Show)

snackCalories :: Elf -> Int
snackCalories = sum . snacks

biggestSnack :: [Elf] -> Int
biggestSnack = maximum . map snackCalories

topSnackHoldersTotal :: Int -> [Elf] -> Int
topSnackHoldersTotal n = sum . take n . sortBy (flip compare) . map snackCalories

parse :: String -> [Elf]
parse = parseLines Nothing . lines

parseLines :: Maybe Elf -> [String] -> [Elf]
parseLines currentElf lines =
  case lines of
    [] -> maybeToList currentElf
    (line : rest) ->
      if line == ""
        then maybe id (:) currentElf (parseLines Nothing rest)
        else case currentElf of
          Just (Elf prevSnacks) ->
            parseLines (Just (Elf (prevSnacks ++ [read line]))) rest
          _ ->
            parseLines (Just (Elf [read line])) rest


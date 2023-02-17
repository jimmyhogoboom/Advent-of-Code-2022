import Data.List
import Data.Maybe (maybeToList)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (file : _) -> do
      content <- readFile file
      let elves = parse content
      part1 elves
      part2 elves
    _ -> print "whoops"

part1 :: [Elf] -> IO ()
part1 elves = do
  putStrLn "\nDay 1.1: Most calories: "
  print $ biggestSnack elves

part2 :: [Elf] -> IO ()
part2 elves = do
  putStrLn "\nDay 1.2: Top 3 snack-holders total: "
  print $ topSnackHoldersTotal elves 3

type Snack = Int

newtype Elf = Elf {snacks :: [Snack]} deriving (Show)

snackCalories :: Elf -> Int
snackCalories = sum . snacks

biggestSnack :: [Elf] -> Int
biggestSnack = maximum . map snackCalories

topSnackHoldersTotal :: [Elf] -> Int -> Int
topSnackHoldersTotal elves n = sum $ take n $ sortBy (flip compare) $ map snackCalories elves

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

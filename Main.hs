import Data.Maybe (maybeToList)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (file : _) -> do
      content <- readFile file
      print $ parse content
    _ -> print "whoops"

type ElfIndex = Int

type Calories = Int

data Elf
  = ElfCalories ElfIndex Calories
  deriving (Show)

parse :: String -> [Elf]
parse = parseLines [] . lines

parseLines :: [Elf] -> [String] -> [Elf]
parseLines elves lines =
  case lines of
    [] -> elves
    (line : rest) -> parseLines (elves ++ [ElfCalories (length elves + 1) 0]) rest

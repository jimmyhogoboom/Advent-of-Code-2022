import Data.Maybe (maybeToList)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (file : _) -> do
      content <- readFile file
      let elves = parse content
      print elves
    _ -> print "whoops"

type Snack = Int

newtype Elf = Elf {snacks :: [Snack]} deriving (Show)

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

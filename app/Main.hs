import Part1.Snacks
import Part2.RPS
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (file : _) -> do
      content <- readFile file
      part1 content
      part2 content
    _ -> print "whoops"

part1 :: String -> IO ()
part1 content = do
  let elves = parse content
  putStrLn "\nDay 1.1: Most calories: "
  print $ biggestSnack elves

part2 :: String -> IO ()
part2 content = do
  let elves = parse content
  putStrLn "\nDay 1.2: Top 3 snack-holders total: "
  print $ topSnackHoldersTotal 3 elves

import Part1.Snacks
import Part2.RPS
import System.Environment (getArgs)

-- TODO: Move input files into their matching Part directories
-- TODO: Have each Part module read its own file
-- TODO: Main should call all parts, printing each part's answer -OR- take a number argument to choose the part to run
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

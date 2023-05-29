import qualified Day1.Snacks as Day1
import qualified Day2.RPS as Day2
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- map readMaybe <$> getArgs
  case args of
    [Just 1, Just 1] ->
      Day1.part1
    [Just 1, Just 2] ->
      Day1.part2
    [Just 2, Just 1] ->
      Day2.part1
    [Just 2, Just 2] ->
      Day2.part2
    [Just x, Just y] ->
      putStrLn "That part isn't done yet"
    _ ->
      putStrLn "Invalid input. Use: `-- 1 1` to run Day 1 Part 1"

import qualified Day1.Snacks as Day1
import qualified Day2.RPS as Day2
import qualified Day3.Ruck as Day3
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- map readMaybe <$> getArgs
  case args of
    [Just x, Just y] ->
      case [x, y] of
        [1, 1] ->
          Day1.part1
        [1, 2] ->
          Day1.part2
        [2, 1] ->
          Day2.part1
        [2, 2] ->
          Day2.part2
        [3, 1] ->
          Day3.part1
        [x, y] ->
          putStrLn "That part isn't done yet"
    _ ->
      putStrLn "Invalid input. Use: `-- 1 1` to run Day 1 Part 1"

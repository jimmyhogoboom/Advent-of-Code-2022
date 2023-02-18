import qualified Day1.Snacks as Day1
import qualified Day2.RPS as Day2
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- map read <$> getArgs
  case args of
    [1, 1] ->
      Day1.part1
    [1, 2] ->
      Day1.part2
    [_, _] -> putStrLn "That part isn't done yet"
    _ -> putStrLn "Invalid input. Use: `-- 1 1` to run Day 1 Part 1"

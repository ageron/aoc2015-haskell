module AOCTools where

import Text.Printf (printf)

runSolution :: (Int, (String -> String, String -> String)) -> IO ()
runSolution (day, (part1, part2)) = do
  putStrLn $ "Day " ++ show day
  input <- readFile (printf "data/day%02d.txt" day)
  putStr "  Part 1: "
  putStrLn $ part1 input
  putStr "  Part 2: "
  putStrLn $ part2 input

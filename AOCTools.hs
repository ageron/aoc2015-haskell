module AOCTools where

import           Control.DeepSeq (deepseq)
import           System.CPUTime (getCPUTime)
import           Text.Printf     (printf)

formatDiffTime :: Integer -> String
formatDiffTime diff
  | seconds < 0.001 = printf "%.0f µs" (seconds * 1000000)
  | seconds < 1 = printf "%.0f ms" (seconds * 1000)
  | otherwise = printf "%.2f s" seconds
  where
    seconds = (fromIntegral diff :: Double) / 1e12

runPart :: (String -> String) -> Int -> String -> IO ()
runPart partFunction index input = do
  putStr $ "  Part " ++ show index ++ ": "
  startTime <- getCPUTime
  let result = partFunction input
  -- deepseq ensures result is actually evaluated before getCPUTime
  endTime <- result `deepseq` getCPUTime
  putStrLn $ result ++ " (" ++ formatDiffTime (endTime - startTime) ++ ")"

runSolution :: (Int, (String -> String, String -> String)) -> IO ()
runSolution (day, (part1, part2)) = do
  putStrLn $ "Day " ++ show day
  input <- readFile (printf "data/day%02d.txt" day)
  runPart part1 1 input
  runPart part2 2 input

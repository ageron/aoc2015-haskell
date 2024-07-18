module AOCTools where

import           Control.DeepSeq (deepseq)
import           Data.Time       (NominalDiffTime, diffUTCTime, getCurrentTime)
import           Text.Printf     (printf)

formatDiffTime :: NominalDiffTime -> String
formatDiffTime diff
  | diff < 0.001 = printf "%.0f µs" (diffd * 1000000)
  | diff < 1 = printf "%.0f ms" (diffd * 1000)
  | otherwise = printf "%.2f s" diffd
  where
    diffd = realToFrac diff :: Double

runPart :: (String -> String) -> Int -> String -> IO ()
runPart partFunction index input = do
  putStr $ "  Part " ++ show index ++ ": "
  startTime <- getCurrentTime
  let result = partFunction input
  -- deepseq ensures result is actually evaluated before getCurrentTime
  endTime <- result `deepseq` getCurrentTime
  putStrLn
    $ result ++ " (" ++ formatDiffTime (diffUTCTime endTime startTime) ++ ")"

runSolution :: (Int, (String -> String, String -> String)) -> IO ()
runSolution (day, (part1, part2)) = do
  putStrLn $ "Day " ++ show day
  input <- readFile (printf "data/day%02d.txt" day)
  runPart part1 1 input
  runPart part2 2 input

import AOCTools
import Day01
import Day02
import System.Environment (getArgs)

solutions :: [(String -> String, String -> String)]
solutions =
  [ (Day01.part1, Day01.part2),
    (Day02.part1, Day02.part2)
  ]

parseDays :: [String] -> [Int]
parseDays [] = [1 .. length solutions]
parseDays args = map read args :: [Int]

main :: IO ()
main = do
  args <- getArgs
  let days = parseDays args
  let dayIndices = map (subtract 1) days
  let selectedSolutions = zip days (map (solutions !!) dayIndices)
  mapM_ runSolution selectedSolutions

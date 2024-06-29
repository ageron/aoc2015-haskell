import AOCTools
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
import System.Environment (getArgs)

solutions :: [(String -> String, String -> String)]
solutions =
  [ (Day01.part1, Day01.part2),
    (Day02.part1, Day02.part2),
    (Day03.part1, Day03.part2),
    (Day04.part1, Day04.part2),
    (Day05.part1, Day05.part2),
    (Day06.part1, Day06.part2),
    (Day07.part1, Day07.part2),
    (Day08.part1, Day08.part2),
    (Day09.part1, Day09.part2),
    (Day10.part1, Day10.part2),
    (Day11.part1, Day11.part2),
    (Day12.part1, Day12.part2),
    (Day13.part1, Day13.part2),
    (Day14.part1, Day14.part2),
    (Day15.part1, Day15.part2),
    (Day16.part1, Day16.part2),
    (Day17.part1, Day17.part2),
    (Day18.part1, Day18.part2),
    (Day19.part1, Day19.part2),
    (Day20.part1, Day20.part2),
    (Day21.part1, Day21.part2),
    (Day22.part1, Day22.part2),
    (Day23.part1, Day23.part2),
    (Day24.part1, Day24.part2),
    (Day25.part1, Day25.part2)
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

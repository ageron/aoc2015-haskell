module Day17 where

parseContainers :: String -> [Int]
parseContainers input = map read (lines input)

combinations :: Int -> [Int] -> [[Int]]
combinations _ [] = []
combinations 0 _ = [[]]
combinations target [x] = [[x] | target == x]
combinations target (x:xs) =
  map (x :) (combinations (target - x) xs) ++ combinations target xs

part1 :: String -> String
part1 input = show $ length $ combinations 150 (parseContainers input)

part2 :: String -> String
part2 input = show $ length $ filter isShortest allCombinations
  where
    allCombinations = combinations 150 (parseContainers input)
    isShortest xs = length xs == minLength
    minLength = minimum $ map length allCombinations

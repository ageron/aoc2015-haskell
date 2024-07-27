module Day24 where

import           Data.Function (on)
import           Data.List     (minimumBy)

parseWeights :: String -> [Int]
parseWeights input = map read (lines input)

groups :: Int -> [Int] -> Int -> [[Int]]
groups _ _ 0 = []
groups _ [] _ = []
groups targetWeight (x:xs) targetLength
  | targetWeight <= 0 || targetLength == 0 = []
  | x == targetWeight && targetLength == 1 = [[x]]
  | x >= targetWeight = groups targetWeight xs targetLength
  | otherwise =
    map (x :) (groups (targetWeight - x) xs (targetLength - 1))
      ++ groups targetWeight xs targetLength

shortestBalancedGroups :: [Int] -> Int -> [[Int]]
shortestBalancedGroups [] _ = []
shortestBalancedGroups weights 1 = [weights]
shortestBalancedGroups weights numGroups =
  head $ filter (not . null) (map validGroupsOfLength [1 ..])
  where
    validGroupsOfLength targetLength =
      filter restCanBeSplitEvenly $ groups optimalWeight weights targetLength
    optimalWeight = sum weights `div` numGroups
    restCanBeSplitEvenly [] = False
    restCanBeSplitEvenly g =
      not $ null $ shortestBalancedGroups (remainingWeights g) (numGroups - 1)
    remainingWeights g = filter (not . (`elem` g)) weights

quantumEntanglementOfPassengerGroup :: String -> Int -> String
quantumEntanglementOfPassengerGroup input numGroups =
  show $ product passengerGroup
  where
    passengerGroup = minimumBy (compare `on` product) candidateGroups
    candidateGroups = shortestBalancedGroups (parseWeights input) numGroups

part1 :: String -> String
part1 input = quantumEntanglementOfPassengerGroup input 3

part2 :: String -> String
part2 input = quantumEntanglementOfPassengerGroup input 4

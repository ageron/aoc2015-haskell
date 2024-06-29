module Day03 where

import Data.Set

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = toList . fromList

type Position = (Int, Int)

path :: String -> [Position]
path = scanl move (0, 0)
  where
    move (x, y) c = case c of
      '<' -> (x - 1, y)
      '>' -> (x + 1, y)
      '^' -> (x, y - 1)
      'v' -> (x, y + 1)
      _ -> error "Invalid direction"

part1 :: String -> String
part1 input = show $ length $ removeDuplicates $ path input

part2 :: String -> String
part2 input = show $ length $ removeDuplicates (path santa ++ path roboSanta)
  where
    santa = [input !! idx | idx <- [0, 2 .. length input - 1]]
    roboSanta = [input !! idx | idx <- [1, 3 .. length input - 1]]

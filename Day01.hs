module Day01 where

import Data.List

floors :: String -> [Int]
floors input = scanl (+) 0 $ map charValue input
  where
    charValue '(' = 1
    charValue ')' = -1
    charValue _ = 0

part1 :: String -> String
part1 input = show $ last $ floors input

part2 :: String -> String
part2 input = maybe "Failed!" show $ findIndex (< 0) $ floors input

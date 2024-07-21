module Day10 where

import           Data.List (group)

repeatLookAndSay :: Int -> String -> String
repeatLookAndSay n input =
  show $ length $ iterate lookAndSay (cleanInput input) !! n
  where
    cleanInput = filter (/= '\n')
    lookAndSay = concatMap say . group
    say xs = show (length xs) ++ [head xs]

part1 :: String -> String
part1 = repeatLookAndSay 40

part2 :: String -> String
part2 = repeatLookAndSay 50

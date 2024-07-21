module Day11 where

import Data.Char (chr, ord)
import Data.List (group, find)
import Data.Maybe (fromJust)

toPassword :: Int -> String
toPassword 0 = []
toPassword n = toPassword (n `div` 26) ++ [chr(ord 'a' + n `mod` 26)]

fromPassword :: String -> Int
fromPassword = foldl value 0
  where
    value acc '\n' = acc
    value acc c = acc * 26 + (ord c - ord 'a')

isValid :: [Char] -> Bool
isValid pwd = all ($ pwd) [hasIncreasingStraight, not . hasIOL, hasTwoPairs]
  where
      hasIncreasingStraight [x,y,z] = ord y == ord x + 1 && ord z == ord y + 1
      hasIncreasingStraight (x:y:z:xs) = hasIncreasingStraight [x,y,z] || hasIncreasingStraight (y:z:xs)
      hasIncreasingStraight _ = False
      hasIOL = any (`elem` "iol")
      hasTwoPairs p = sum (filter (>= 2) (groupLengths p)) >= 4
      groupLengths p = map length $ group p

nextPassword :: String -> String
nextPassword pwd = fromJust $ find isValid $ map toPassword [(fromPassword pwd + 1)..]

part1 :: String -> String
part1 = nextPassword

part2 :: String -> String
part2 = nextPassword . nextPassword

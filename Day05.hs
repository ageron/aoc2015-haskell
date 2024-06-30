module Day05 where

import Data.Char (toLower)
import Text.Regex.PCRE ((=~))

part1 :: String -> String
part1 input = show $ length $ filter niceString (lines input)
  where
    niceString str = hasThreeVowels str && hasDoubleLetter str && not (hasIllegal str)
      where
        hasThreeVowels s = countVowels s >= 3
          where
            isVowel ch = toLower ch `elem` "aeiou"
            countVowels = length . filter isVowel
        hasDoubleLetter s = s =~ "(.)\\1"
        hasIllegal s = s =~ "ab|cd|pq|xy"

part2 :: String -> String
part2 input = show $ length $ filter niceString (lines input)
  where
    niceString str = hasTwoPairs str && hasSandwich str
      where
        hasTwoPairs s = s =~ "(..).*\\1"
        hasSandwich s = s =~ "(.).\\1"

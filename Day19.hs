module Day19 where

import           Data.Char (isUpper)
import           Data.List (isPrefixOf, nub)

parseChemistry :: String -> ([([String], [String])], [String])
parseChemistry input = (chemistry, target)
  where
    lines_ = lines input
    chemistry = concatMap (parseLine . words) lines_
    parseLine [i, "=>", o] = [(splitElements i, splitElements o)]
    parseLine _            = []
    target = splitElements $ last lines_
    splitElements [] = []
    splitElements [x] = [[x]]
    splitElements (x:y:xs)
      | isUpper y = [x] : splitElements (y : xs)
      | otherwise = [x, y] : splitElements xs

neighbors :: [String] -> [([String], [String])] -> [[String]]
neighbors molecule chemistry = nub $ concatMap (replacements molecule) chemistry
  where
    replacements [] _ = []
    replacements mol (i, o) =
      (if i `isPrefixOf` mol
         then thisReplacement
         else [])
        ++ nextReplacements
      where
        thisReplacement = [o ++ drop (length i) mol]
        nextReplacements = map (head mol :) (replacements (tail mol) (i, o))

part1 :: String -> String
part1 input = show $ length $ neighbors target chemistry
  where
    (chemistry, target) = parseChemistry input

-- I tried using A* but the combinatorial explosion was too great, even when
-- going in reverse (i.e., from the target molecule to the electron).
-- So I ended up searching for patterns in the chemistry, and I noticed
-- that most replacements only add a single element, except some that
-- add a few more, and these all end in Ar. Moreover, the longer
-- ones have more Y's, and this led to the following simple equation, which
-- worked. However, it's really specific to my chemistry, you may need to
-- replace Ar and Y with other elements, and perhaps also the multiplier.
part2 :: String -> String
part2 input = show minReplacements
  where
    minReplacements = length target - 2 * (count "Y" + count "Ar") - 1
    (_, target) = parseChemistry input
    count element = length $ filter (== element) target

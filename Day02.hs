module Day02 where

import           Data.List.Split (splitOn)

boxSizes :: String -> [(Int, Int, Int)]
boxSizes input = map parseBoxSize (lines input)
  where
    parseBoxSize str =
      case splitOn "x" str of
        [l, w, h] -> (read l, read w, read h)
        _         -> error "Expected format <l>x<w>x<h>"

part1 :: String -> String
part1 input = show $ sum $ map paperArea (boxSizes input)
  where
    paperArea (l, w, h) = sum (map (* 2) boxSideAreas) + minimum boxSideAreas
      where
        boxSideAreas = [l * w, w * h, h * l]

part2 :: String -> String
part2 input = show $ sum $ map ribbonLength (boxSizes input)
  where
    ribbonLength (l, w, h) = 2 * (l + w + h - maximum [l, w, h]) + l * w * h

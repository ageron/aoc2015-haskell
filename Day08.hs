module Day08 where

part1 :: String -> String
part1 input = show $ sum $ map ((+ 2) . unescapedDiff) (lines input)
  where
    unescapedDiff []                = 0 :: Int
    unescapedDiff [_]               = 0
    unescapedDiff ('\\':'\"':xs)    = 1 + unescapedDiff xs
    unescapedDiff ('\\':'\\':xs)    = 1 + unescapedDiff xs
    unescapedDiff ('\\':'x':_:_:xs) = 3 + unescapedDiff xs
    unescapedDiff (_:xs)            = unescapedDiff xs

part2 :: String -> String
part2 input = show $ sum $ map escapedDiff (lines input)
  where
    escapedDiff line = 2 + length (filter (`elem` "\"\\") line)

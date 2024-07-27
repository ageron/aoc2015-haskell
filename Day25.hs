module Day25 where

parseCoordinates :: String -> (Int, Int)
parseCoordinates = toPair . map read . words . filter (`elem` "0123456789 ")
  where
    toPair [row, col] = (row, col)
    toPair _          = error "Invalid coordinates format"

codeByIndex :: Int -> Int
codeByIndex 1 = 20151125
codeByIndex n = (codeByIndex (n - 1) * 252533) `mod` 33554393

index :: (Int, Int) -> Int
index (1, col)   = col * (col + 1) `div` 2 -- 1 + 2 + ... + col
index (row, col) = index (1, col + row - 1) - (row - 1)

part1 :: String -> String
part1 = show . codeByIndex . index . parseCoordinates

part2 :: String -> String
part2 _ = "The end"

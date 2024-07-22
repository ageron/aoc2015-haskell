module Day18 where

parseGrid :: String -> [[Bool]]
parseGrid input = map (map (== '#')) (lines input)

step :: Bool -> [[Bool]] -> [[Bool]]
step cornersOn grid =
  [[stepCell x y | x <- [0 .. size - 1]] | y <- [0 .. size - 1]]
  where
    stepCell x y =
      (if cell x y
         then (neighbors x y - 1) `elem` [2, 3]
         else neighbors x y == 3)
        || (cornersOn && isCorner x y)
    cell x y = (inGrid x y && (grid !! y !! x)) || (cornersOn && isCorner x y)
    inGrid x y = x >= 0 && x < size && y >= 0 && y < size
    isCorner x y = (x == 0 || x == size - 1) && (y == 0 || y == size - 1)
    neighbors x y =
      countLights [[cell i j | i <- [x - 1 .. x + 1]] | j <- [y - 1 .. y + 1]]
    size = length grid

countLights :: [[Bool]] -> Int
countLights grid = length $ concatMap (filter id) grid

runAndCountLights :: Bool -> String -> String
runAndCountLights cornersOn input = show $ countLights $ gameOfLife !! numSteps
  where
    gameOfLife = iterate (step cornersOn) (parseGrid input)
    numSteps = 100

part1 :: String -> String
part1 = runAndCountLights False

part2 :: String -> String
part2 = runAndCountLights True

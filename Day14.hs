module Day14 where

data Reindeer = Reindeer
  { name     :: String
  , speed    :: Int
  , duration :: Int
  , rest     :: Int
  } deriving (Show)

parseReindeers :: String -> [Reindeer]
parseReindeers input = map (parseLine . words) (lines input)
  where
    parseLine [name, "can", "fly", speedStr, "km/s", "for", durationStr, "seconds,", "but", "then", "must", "rest", "for", restStr, "seconds."] =
      Reindeer
        { name
        , speed = read speedStr
        , duration = read durationStr
        , rest = read restStr
        }
    parseLine _ = error "Invalid reindeer list format"

distance :: Int -> Reindeer -> Int
distance time reindeer =
  numCycles * cycleDistance + latestFlyingDuration * speed reindeer
  where
    numCycles = time `div` cycleDuration
    cycleDistance = speed reindeer * duration reindeer
    latestFlyingDuration = min (duration reindeer) (time `mod` cycleDuration)
    cycleDuration = duration reindeer + rest reindeer

points :: [Reindeer] -> Int -> [Int]
points reindeers time = foldl updatePoints zeroPoints [1 .. time]
  where
    zeroPoints = replicate (length reindeers) 0
    updatePoints currentPoints t = zipWith newPoints currentPoints distances
      where
        distances = map (distance t) reindeers
        maxDistance = maximum distances
        newPoints point dist =
          point
            + if dist == maxDistance
                then 1
                else 0

endTime :: Int
endTime = 2503

part1 :: String -> String
part1 input = show $ maximum $ map (distance endTime) (parseReindeers input)

part2 :: String -> String
part2 input = show $ maximum $ points (parseReindeers input) endTime

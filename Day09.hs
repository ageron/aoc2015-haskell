module Day09 where

import           Data.List  (nub, permutations)
import qualified Data.Map   as Map
import           Data.Maybe (mapMaybe)

parseDistances :: String -> Map.Map (String, String) Int
parseDistances input =
  Map.fromList $ concatMap (parseLine . words) (lines input)
  where
    parseLine [loc1, "to", loc2, "=", dist] =
      let d = read dist :: Int
       in [((loc1, loc2), d), ((loc2, loc1), d)]
    parseLine _ = error "Invalid format"

allPathLengths :: String -> [Int]
allPathLengths input = mapMaybe pathLength allPaths
  where
    pathLength path = sum <$> mapM distance (toPairs path)
    distance pair = Map.lookup pair distances
    distances = parseDistances input
    toPairs path = zip path (drop 1 path)
    allPaths = permutations locations
    locations = nub $ map fst $ Map.keys distances

part1 :: String -> String
part1 input = show $ minimum $ allPathLengths input

part2 :: String -> String
part2 input = show $ maximum $ allPathLengths input

module Day16 where

import           Data.List  (find)
import qualified Data.Map   as Map
import           Data.Maybe (fromJust)

type Compounds = Map.Map String Int

type Aunt = (Int, Compounds)

parseAunts :: String -> [Aunt]
parseAunts input = map (parseLine . words) (lines cleanInput)
  where
    cleanInput = filter (not . (`elem` ",:")) input
    parseLine ("Sue":indexStr:compounds) =
      (read indexStr, Map.fromList (parseCompounds compounds))
    parseLine _ = error "Invalid ingredient list format"
    parseCompounds [] = []
    parseCompounds (name:valueStr:xs) =
      (name, read valueStr) : parseCompounds xs
    parseCompounds _ = error "Invalid format"

condition :: Int -> String -> Int -> Bool
condition part name =
  flip
    ((if part == 1
        then fst
        else snd)
       operators)
    value
  where
    (value, operators) = targets name
    targets "children"    = (3, ((==), (==)))
    targets "cats"        = (7, ((==), (>)))
    targets "samoyeds"    = (2, ((==), (==)))
    targets "pomeranians" = (3, ((==), (<)))
    targets "akitas"      = (0, ((==), (==)))
    targets "vizslas"     = (0, ((==), (==)))
    targets "goldfish"    = (5, ((==), (<)))
    targets "trees"       = (3, ((==), (>)))
    targets "cars"        = (2, ((==), (==)))
    targets "perfumes"    = (1, ((==), (==)))
    targets _             = error "Invalid compound"

findAunt :: Int -> String -> String
findAunt part input = show $ fst $ fromJust $ find matching aunts
  where
    aunts = parseAunts input
    matching (_, compounds) = all isValid (Map.toList compounds)
    isValid (name, value) = condition part name value

part1 :: String -> String
part1 = findAunt 1

part2 :: String -> String
part2 = findAunt 2

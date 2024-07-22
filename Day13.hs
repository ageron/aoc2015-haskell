module Day13 where

import           Data.List  (nub, permutations)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)

parseGuestList :: String -> Map.Map (String, String) Int
parseGuestList input = Map.fromList $ map (parseLine . words) (lines input)
  where
    parseLine [guest1, "would", signStr, valueStr, "happiness", "units", "by", "sitting", "next", "to", guest2] =
      let guest2clean = filter (/= '.') guest2
          sign "gain" = 1
          sign "lose" = -1
          sign _      = parseError
          value = sign signStr * (read valueStr :: Int)
       in ((guest1, guest2clean), value)
    parseLine _ = parseError
    parseError = error "Invalid guest list format"

maxHappinessChange :: Bool -> Map.Map (String, String) Int -> Int
maxHappinessChange includeYourself guestList =
  maximum $ map totalHappinessChange seatings
  where
    seatings =
      permutations
        (if includeYourself
           then "" : guests
           else guests)
    guests = nub $ concatMap (\(a, b) -> [a, b]) (Map.keys guestList)
    totalHappinessChange seating =
      sum
        $ map happinessChangeBothWays
        $ (last seating, head seating) : zip seating (drop 1 seating)
    happinessChangeBothWays (a, b) =
      happinessChange (a, b) + happinessChange (b, a)
    happinessChange pair = fromMaybe 0 $ Map.lookup pair guestList

part1 :: String -> String
part1 input = show $ maxHappinessChange False (parseGuestList input)

part2 :: String -> String
part2 input = show $ maxHappinessChange True (parseGuestList input)

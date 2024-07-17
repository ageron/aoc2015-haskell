module Day06 where

import Text.Regex.PCRE ((=~))
import Data.Array

data Action = TurnOn | TurnOff | Toggle deriving (Eq, Show)

parseAction :: String -> Action
parseAction "turn on" = TurnOn
parseAction "turn off" = TurnOff
parseAction "toggle" = Toggle
parseAction s = error $ "Invalid action '" ++ s ++ "'"

parseInstructions :: String -> [(Action, [Int])]
parseInstructions input = map parseInstruction $ lines input
  where
    parseInstruction line = instructionTuple (head $ parseGroups line)
    instructionTuple (_ : actionStr : coordsStr) = (parseAction actionStr, map read coordsStr :: [Int])
    instructionTuple gs = error $ "Invalid instruction " ++ show gs
    parseGroups line =
      let regex = "(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)"
       in (line =~ regex) :: [[String]]

lightsOff :: Array (Int, Int) Int
lightsOff = accumArray (+) 0 ((0, 0), (999, 999)) []

runInstructions :: (Action -> Int -> Int) -> String -> String
runInstructions updateRule input = show $ sum $ foldl updateLights lightsOff (parseInstructions input)
  where
    updateLights lights (action, [x1,y1,x2,y2]) =
      lights // [(idx, updateRule action (lights!idx)) | idx <- range ((x1, y1), (x2, y2))]
    updateLights _ _ = error "Invalid instructions"

part1 :: String -> String
part1 = runInstructions updateRule
  where
    updateRule TurnOn _ = 1
    updateRule TurnOff _ = 0
    updateRule Toggle oldState = 1 - oldState

part2 :: String -> String
part2 = runInstructions updateRule
  where
    updateRule TurnOn oldState = oldState + 1
    updateRule TurnOff oldState = max 0 (oldState - 1)
    updateRule Toggle oldState = oldState + 2

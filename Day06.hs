module Day06 where

import           Data.Array        (range)
import           Data.Array.IArray (elems)
import           Data.Array.MArray (newArray, readArray, writeArray)
import           Data.Array.ST     (runSTUArray)
import           Data.Foldable     (for_)
import           Text.Regex.PCRE   ((=~))

data Action
  = TurnOn
  | TurnOff
  | Toggle
  deriving (Eq, Show)

parseAction :: String -> Action
parseAction "turn on"  = TurnOn
parseAction "turn off" = TurnOff
parseAction "toggle"   = Toggle
parseAction action     = error $ "Invalid action '" ++ action ++ "'"

parseInstructions :: String -> [(Action, ((Int, Int), (Int, Int)))]
parseInstructions input = map parseInstruction $ lines input
  where
    parseInstruction line = instructionTuple (head $ parseGroups line)
      where
        instructionTuple (_:actionStr:coordsStr) =
          (parseAction actionStr, parseCoords (map read coordsStr))
          where
            parseCoords [x1, y1, x2, y2] = ((x1, y1), (x2, y2))
            parseCoords _                = error "Invalid coordinates"
        instructionTuple gs = error $ "Invalid instruction " ++ show gs
        parseGroups s =
          let regex =
                "(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)"
           in (s =~ regex) :: [[String]]

runInstructions :: (Action -> Int -> Int) -> String -> String
runInstructions updateRule input =
  let actions = parseInstructions input
   in show
        $ sum
        $ elems
        $ runSTUArray
        $ do
            lightsArray <- newArray ((0, 0), (999, 999)) 0
            for_ actions $ \(action, coords) ->
              for_ (range coords) $ \idx -> do
                oldState <- readArray lightsArray idx
                writeArray lightsArray idx (updateRule action oldState)
            return lightsArray

part1 :: String -> String
part1 = runInstructions updateRule
  where
    updateRule TurnOn _        = 1
    updateRule TurnOff _       = 0
    updateRule Toggle oldState = 1 - oldState

part2 :: String -> String
part2 = runInstructions updateRule
  where
    updateRule TurnOn oldState  = oldState + 1
    updateRule TurnOff oldState = max 0 (oldState - 1)
    updateRule Toggle oldState  = oldState + 2

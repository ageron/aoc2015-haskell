module Day07 where

import           Data.Bits  (complement, shiftL, shiftR, (.&.), (.|.))
import           Data.Char  (isDigit)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)
import           Data.Word  (Word16)

data Component
  = Signal Word16
  | Gate
      { func  :: Word16 -> Word16 -> Word16
      , left  :: Component
      , right :: Component
      }
  | NotGate Component
  | Wire String

type Instructions = Map.Map String Component

type OutputSignals = Map.Map String Word16

outputSignals :: Instructions -> OutputSignals
outputSignals insts = outSignals
  where
    outSignals = Map.map outputSignal insts
      where
        outputSignal (Signal x) = x
        outputSignal (Gate func left right) =
          outputSignal left `func` outputSignal right
        outputSignal (NotGate x) = complement (outputSignal x)
        outputSignal (Wire x) =
          fromMaybe (error "Invalid circuit") (Map.lookup x outSignals)

instructions :: String -> Instructions
instructions input = Map.fromList $ parseInstructions input
  where
    parseInstructions s = map (parseInstruction . words) (lines s)
    parseInstruction [x, "OR", y, "->", n] = (n, binaryGate (.|.) x y)
    parseInstruction [x, "AND", y, "->", n] = (n, binaryGate (.&.) x y)
    parseInstruction [x, "RSHIFT", y, "->", n] = (n, binaryGate shiftRight x y)
    parseInstruction [x, "LSHIFT", y, "->", n] = (n, binaryGate shiftLeft x y)
    parseInstruction ["NOT", x, "->", n] = (n, NotGate $ parseComponent x)
    parseInstruction [x, "->", n] = (n, parseComponent x)
    parseInstruction s = error $ "Invalid instruction: " ++ unwords s
    shiftRight x y = shiftR x $ fromIntegral y
    shiftLeft x y = shiftL x $ fromIntegral y
    binaryGate func x y =
      Gate {func, left = parseComponent x, right = parseComponent y}
    parseComponent s
      | all isDigit s = Signal (read s)
      | otherwise = Wire s

outputWireA :: Instructions -> Word16
outputWireA insts =
  fromMaybe (error "Invalid circuit") $ Map.lookup "a" (outputSignals insts)

part1 :: String -> String
part1 input = show $ outputWireA (instructions input)

part2 :: String -> String
part2 input = show $ outputWireA updatedInstructions
  where
    updatedInstructions = Map.insert "b" (Signal (outputWireA insts)) insts
    insts = instructions input

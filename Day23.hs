module Day23 where

data Register
  = RegA
  | RegB
  deriving (Show)

data Instruction
  = Hlf Register
  | Inc Register
  | Tpl Register
  | Jmp Int
  | Jie Register Int
  | Jio Register Int
  deriving (Show)

parseInstructions :: String -> [Instruction]
parseInstructions input = map (parseInstruction . words) (lines cleanInput)
  where
    cleanInput = filter (not . (`elem` ",+")) input
    parseInstruction ["hlf", [reg]]         = Hlf (register reg)
    parseInstruction ["inc", [reg]]         = Inc (register reg)
    parseInstruction ["tpl", [reg]]         = Tpl (register reg)
    parseInstruction ["jmp", offset]        = Jmp (read offset)
    parseInstruction ["jie", [reg], offset] = Jie (register reg) (read offset)
    parseInstruction ["jio", [reg], offset] = Jio (register reg) (read offset)
    parseInstruction _                      = error "Invalid instruction format"
    register 'a' = RegA
    register 'b' = RegB
    register _   = error "Invalid register format"

runProgram :: [Instruction] -> ((Int, Int), Int) -> Int
runProgram instructions (registers, index)
  | index >= length instructions = snd registers
  | otherwise = runProgram instructions updatedState
  where
    updatedState = runInstruction $ instructions !! index
    runInstruction (Hlf reg)        = (updateReg reg (`div` 2), index + 1)
    runInstruction (Inc reg)        = (updateReg reg (+ 1), index + 1)
    runInstruction (Tpl reg)        = (updateReg reg (* 3), index + 1)
    runInstruction (Jmp offset)     = (registers, index + offset)
    runInstruction (Jie reg offset) = conditionalJump even reg offset
    runInstruction (Jio reg offset) = conditionalJump (== 1) reg offset
    conditionalJump condition reg offset =
      if condition (value reg)
        then (registers, index + offset)
        else (registers, index + 1)
    value RegA = fst registers
    value RegB = snd registers
    updateReg RegA op = (op (value RegA), value RegB)
    updateReg RegB op = (value RegA, op (value RegB))

part1 :: String -> String
part1 input = show $ runProgram (parseInstructions input) ((0, 0), 0)

part2 :: String -> String
part2 input = show $ runProgram (parseInstructions input) ((1, 0), 0)

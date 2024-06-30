module Day04 where

import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.UTF8 as BSU
import Data.List

isValidHash :: String -> Int -> Int -> Bool
isValidHash input difficulty index = BSU.take difficulty h == nZeros difficulty
  where
    h = encode . MD5.hash . BSU.fromString $ cleanInput ++ show index
    nZeros n = BSU.fromString $ replicate n '0'
    cleanInput = filter (/= '\n') input

mine :: String -> Int -> String
mine input difficulty = maybe "Failed!" show index
  where
    index = find (isValidHash input difficulty) [1 ..]

part1 :: String -> String
part1 input = mine input 5

part2 :: String -> String
part2 input = mine input 6

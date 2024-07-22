module Day12 where

import           Data.Aeson                 (Value (Array, Number, Object, String),
                                             decode)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson.KeyMap as KM
import           Data.Maybe                 (fromJust)
import           Data.Scientific            (toRealFloat)
import qualified Data.Text                  as T
import qualified Data.Vector                as V

sumAllNumbers :: Bool -> Value -> Int
sumAllNumbers _ (Number n) = round (toRealFloat n :: Double)
sumAllNumbers rejectRed (Object o) =
  if rejectRed && hasRed
    then 0
    else sum $ map (sumAllNumbers rejectRed) values
  where
    values = KM.elems o
    hasRed = String (T.pack "red") `elem` values
sumAllNumbers rejectRed (Array a) = sum $ V.map (sumAllNumbers rejectRed) a
sumAllNumbers _ _ = 0

santaAccounting :: Bool -> String -> String
santaAccounting rejectRed input = show $ sumAllNumbers rejectRed json
  where
    json = fromJust (decode (B.pack input) :: Maybe Value)

part1 :: String -> String
part1 = santaAccounting False

part2 :: String -> String
part2 = santaAccounting True
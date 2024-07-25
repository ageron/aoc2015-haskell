module Day20 where

import           Data.List (find, group)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n
  | even n = 2 : primeFactors (n `div` 2)
  | otherwise = primeFactors' n 3
  where
    primeFactors' m d
      | m == 1 = []
      | d * d > m = [m]
      | m `mod` d == 0 = d : primeFactors' (m `div` d) d
      | otherwise = primeFactors' m (d + 2)

divisors :: Int -> [Int]
divisors n = map product (sequence singlePrimeDivisors)
  where
    groupedPrimeFactors = group (primeFactors n)
    singlePrimeDivisors =
      [scanl (*) 1 primeGroup | primeGroup <- groupedPrimeFactors]

numPresents1 :: Int -> Int
numPresents1 h = 10 * sum (divisors h)

numPresents2 :: Int -> Int
numPresents2 h = 11 * sum (filter ((>= h) . (* 50)) (divisors h))

part1 :: String -> String
part1 input = show $ find ((>= target) . numPresents1) [1 ..]
  where
    target = read input

part2 :: String -> String
part2 input = show $ find ((>= target) . numPresents2) [1 ..]
  where
    target = read input

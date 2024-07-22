module Day15 where

import           Data.List (transpose)

parseIngredients :: String -> [[Int]]
parseIngredients input = map (parseLine . words) (lines cleanInput)
  where
    cleanInput = filter (not . (`elem` ",:")) input
    parseLine [_, "capacity", capacityStr, "durability", durabilityStr, "flavor", flavorStr, "texture", textureStr, "calories", caloriesStr] =
      map read [caloriesStr, capacityStr, durabilityStr, flavorStr, textureStr]
    parseLine _ = error "Invalid ingredient list format"

recipes :: Int -> Int -> [[Int]]
recipes 1 numTeaspoons = [[numTeaspoons]]
recipes numIngredients numTeaspoons =
  [ x : xs
  | x <- [0 .. numTeaspoons]
  , xs <- recipes (numIngredients - 1) (numTeaspoons - x)
  ]

score :: [[Int]] -> [Int] -> Int
score ingredients recipe = product propertySums
  where
    propertySums = map (notNegative . sum) (transpose totalIngredients)
    notNegative = max 0
    totalIngredients = zipWith multiplyIngredients recipe ingredients
    multiplyIngredients num ingredient = map (* num) (tail ingredient)

findBestScore :: Int -> String -> String
findBestScore requiredCalories input =
  show $ maximum $ map (score ingredients) filteredRecipes
  where
    filteredRecipes =
      if requiredCalories > 0
        then filter fixedCalories possibleRecipes
        else possibleRecipes
    possibleRecipes = recipes (length ingredients) 100
    ingredients = parseIngredients input
    fixedCalories recipe = recipeCalories recipe == requiredCalories
    ingredientCalories = map head ingredients
    recipeCalories recipe = sum $ zipWith (*) recipe ingredientCalories

part1 :: String -> String
part1 = findBestScore 0

part2 :: String -> String
part2 = findBestScore 500

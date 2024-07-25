module Day21 where

data Player = Player
  { hp     :: Int
  , damage :: Int
  , armor  :: Int
  } deriving (Show)

parseBoss :: String -> Player
parseBoss input = newPlayer $ map (read . last . words) (lines input)
  where
    newPlayer [hp, damage, armor] = Player {hp, damage, armor}
    newPlayer _                   = error "Invalid boss stats"

-- True = player wins, False = boss wins
fight :: Player -> Player -> Bool
fight player boss
  | hp player <= 0 = False
  | otherwise = not $ fight (boss {hp = hp boss - hits}) player
  where
    hits = max 1 $ damage player - armor boss

data Item = Item
  { cost       :: Int
  , itemDamage :: Int
  , itemArmor  :: Int
  } deriving (Eq, Show)

weapons :: [Item]
weapons =
  [ Item 8 4 0 -- Dagger
  , Item 10 5 0 -- Shortsword
  , Item 25 6 0 -- Warhammer
  , Item 40 7 0 -- Longsword
  , Item 74 8 0 -- Greataxe
  ]

armors :: [Item]
armors =
  [ Item 0 0 0 -- <No armor>
  , Item 13 0 1 -- Leather
  , Item 31 0 2 -- Chainmail
  , Item 53 0 3 -- Splintmail
  , Item 75 0 4 -- Bandedmail
  , Item 102 0 5 -- Platemail
  ]

rings :: [Item]
rings =
  [ Item 0 0 0 -- <No ring>
  , Item 25 1 0 -- Damage +1
  , Item 50 2 0 -- Damage +2
  , Item 100 3 0 -- Damage +3
  , Item 20 0 1 -- Defense +1
  , Item 40 0 2 -- Defense +2
  , Item 80 0 3 -- Defense +3
  ]

possibleInventories :: [[Item]]
possibleInventories =
  filter noIdenticalRings $ sequence [weapons, armors, rings, rings]
  where
    noIdenticalRings [_, _, Item 0 0 0, Item 0 0 0] = True -- no rings at all
    noIdenticalRings [_, _, ring1, ring2]           = ring1 /= ring2 -- one ring or two different rings
    noIdenticalRings _                              = error "Unreachable code"

preparePlayer :: [Item] -> Player
preparePlayer inventory =
  Player {hp = 100, damage = totalDamage, armor = totalArmor}
  where
    totalDamage = sum $ map itemDamage inventory
    totalArmor = sum $ map itemArmor inventory

possibleCosts :: Bool -> String -> [Int]
possibleCosts playerWins input = map totalCost selectedInventories
  where
    totalCost inventory = sum $ map cost inventory
    selectedInventories = filter selectInventory possibleInventories
    selectInventory inventory =
      fight (preparePlayer inventory) boss == playerWins
    boss = parseBoss input

part1 :: String -> String
part1 input = show $ minimum $ possibleCosts True input

part2 :: String -> String
part2 input = show $ maximum $ possibleCosts False input

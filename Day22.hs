module Day22 where

parseBoss :: String -> (Int, Int)
parseBoss input = toPair $ map (read . last . words) (lines input)
  where
    toPair [hp, damage] = (hp, damage)
    toPair _            = error "Invalid boss format"

data GamePhase
  = EffectsPhase1
  | CastPhase
  | EffectsPhase2
  | BossPhase
  deriving (Eq, Show)

data GameState = GameState
  { hp            :: Int
  , bossHp        :: Int
  , bossDamage    :: Int
  , manaLeft      :: Int
  , manaSpent     :: Int
  , shieldTimer   :: Int
  , poisonTimer   :: Int
  , rechargeTimer :: Int
  , isHardMode    :: Bool
  } deriving (Show)

data Spell
  = MagicMissile
  | Drain
  | Shield
  | Poison
  | Recharge
  deriving (Show)

playOneRound :: GameState -> Spell -> GameState
playOneRound initState spell =
  foldl playPhase initState [EffectsPhase1, CastPhase, EffectsPhase2, BossPhase]
  where
    playPhase :: GameState -> GamePhase -> GameState
    playPhase state phase'
      | manaLeft state < 0 = state {hp = 0} -- out of mana, player lost
      | hp state <= 0 || bossHp state <= 0 = state -- player or boss lost
      | phase' `elem` [EffectsPhase1, EffectsPhase2] = doEffectsPhase
      | phase' == CastPhase = doCastPhase spell
      | phase' == BossPhase = doBossPhase
      | otherwise = error "Unreachable code"
      where
        doEffectsPhase :: GameState
        doEffectsPhase =
          state
            { hp =
                hp state
                  - if isHardMode state && phase' == EffectsPhase1
                      then 1
                      else 0
            , bossHp =
                bossHp state
                  - if poisonTimer state > 0
                      then 3
                      else 0
            , manaLeft =
                manaLeft state
                  + if rechargeTimer state > 0
                      then 101
                      else 0
            , shieldTimer = max 0 (shieldTimer state - 1)
            , poisonTimer = max 0 (poisonTimer state - 1)
            , rechargeTimer = max 0 (rechargeTimer state - 1)
            }
        doBossPhase :: GameState
        doBossPhase =
          state
            { hp =
                hp state
                  - max
                      1 -- at least 1 damage
                      (bossDamage state
                         - if shieldTimer state > 0
                             then 7
                             else 0)
            }
        doCastPhase :: Spell -> GameState
        doCastPhase MagicMissile =
          state
            { manaLeft = manaLeft state - 53
            , manaSpent = manaSpent state + 53
            , bossHp = bossHp state - 4
            }
        doCastPhase Drain =
          state
            { manaLeft = manaLeft state - 73
            , manaSpent = manaSpent state + 73
            , hp = hp state + 2
            , bossHp = bossHp state - 2
            }
        doCastPhase Shield =
          state
            { manaLeft = manaLeft state - 113
            , manaSpent = manaSpent state + 113
            , hp = checkTimer shieldTimer
            , shieldTimer = 6
            }
        doCastPhase Poison =
          state
            { manaLeft = manaLeft state - 173
            , manaSpent = manaSpent state + 173
            , hp = checkTimer poisonTimer
            , poisonTimer = 6
            }
        doCastPhase Recharge =
          state
            { manaLeft = manaLeft state - 229
            , manaSpent = manaSpent state + 229
            , hp = checkTimer rechargeTimer
            , rechargeTimer = 5
            }
        checkTimer timer =
          if timer state > 0
            then 0
            else hp state

minimumManaSpentToWin :: GameState -> Int -> Int
minimumManaSpentToWin state bestManaSpentSoFar =
  foldl
    minManaStartingWithSpell
    bestManaSpentSoFar
    [MagicMissile, Drain, Shield, Poison, Recharge]
  where
    minManaStartingWithSpell bestMana spell
      | hp state <= 0 || manaSpent state >= bestMana = bestMana
      | bossHp state <= 0 = min (manaSpent state) bestMana
      | otherwise = minimumManaSpentToWin (playOneRound state spell) bestMana

initGameState :: (Int, Int) -> Bool -> GameState
initGameState (bossHp, bossDamage) isHardMode =
  GameState
    { hp = 50
    , bossHp
    , bossDamage
    , manaLeft = 500
    , manaSpent = 0
    , shieldTimer = 0
    , poisonTimer = 0
    , rechargeTimer = 0
    , isHardMode
    }

part1 :: String -> String
part1 input = show $ minimumManaSpentToWin initState maxBound
  where
    initState = initGameState (parseBoss input) False

part2 :: String -> String
part2 input = show $ minimumManaSpentToWin initState maxBound
  where
    initState = initGameState (parseBoss input) True

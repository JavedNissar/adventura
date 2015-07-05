module Enemies
(
  Enemy
  ,weakEnemies
  ,strongEnemies
  ,bosses
) where

data Enemy = Enemy {healthpoints :: Int,enemyDamage :: Int,enemyHitChance:: Float, rewardScore:: Int, enemyName :: String} deriving (Show,Eq)

parademon = Enemy 10 10 0.5 10 "Parademon"
goblin = Enemy 20 20 0.3 10 "Goblin"
tinyMinotaur = Enemy 30 30 0.1 10 "Tiny Minotaur"

apokoliptian = Enemy 50 50 0.3 100 "Apokoliptian"
orc = Enemy 40 40 0.4 100 "Orc"
largeMinotaur = Enemy 30 30 0.5 100 "Large Minotaur"

darkGod = Enemy 100 70 0.5 10000 "Dark God"
warboss = Enemy 70 50 0.7 1000 "Orc Boss"
minotaurKing = Enemy 80 30 0.9 1000 "Minotaur King"

weakEnemies=[parademon,goblin,tinyMinotaur]
strongEnemies=[apokoliptian,orc,largeMinotaur]
bosses=[darkGod,warboss,minotaurKing]

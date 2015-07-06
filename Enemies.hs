module Enemies
(
  Enemy(..)
  ,parademon
  ,goblin
  ,tinyMinotaur
  ,apokoliptian
  ,orc
  ,largeMinotaur
  ,darkGod
  ,warboss
  ,minotaurKing
  ,weakEnemies
  ,strongEnemies
  ,bosses
  ,enemyIsDead
  ,deductHealthFromEnemy
) where

data Enemy = Enemy {healthpoints :: Int,enemyDamage :: Int,enemyHitChance:: Float, enemyName :: String} deriving (Show,Eq)

parademon = Enemy 5 10 0.5 "Parademon"
goblin = Enemy 10 20 0.3 "Goblin"
tinyMinotaur = Enemy 20 30 0.1 "Tiny Minotaur"

apokoliptian = Enemy 25 30 0.3 "Apokoliptian"
orc = Enemy 28 20 0.4 "Orc"
largeMinotaur = Enemy 30 30 0.5 "Large Minotaur"

darkGod = Enemy 40 40 0.5 "Dark God"
warboss = Enemy 30 30 0.7 "Orc Boss"
minotaurKing = Enemy 35 30 0.9 "Minotaur King"

weakEnemies=[parademon,goblin,tinyMinotaur]
strongEnemies=[apokoliptian,orc,largeMinotaur]
bosses=[darkGod,warboss,minotaurKing]

deductHealthFromEnemy::Enemy->Int->Enemy
deductHealthFromEnemy enemy deduction=Enemy ((healthpoints enemy)-deduction) (enemyDamage enemy) (enemyHitChance enemy) (enemyName enemy)

enemyIsDead::Enemy->Bool
enemyIsDead enemy=(healthpoints enemy)<=0

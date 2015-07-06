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

parademon = Enemy 10 10 0.5 "Parademon"
goblin = Enemy 20 20 0.3 "Goblin"
tinyMinotaur = Enemy 30 30 0.1 "Tiny Minotaur"

apokoliptian = Enemy 50 50 0.3 "Apokoliptian"
orc = Enemy 40 40 0.4 "Orc"
largeMinotaur = Enemy 30 30 0.5 "Large Minotaur"

darkGod = Enemy 100 70 0.5 "Dark God"
warboss = Enemy 70 50 0.7 "Orc Boss"
minotaurKing = Enemy 80 30 0.9 "Minotaur King"

weakEnemies=[parademon,goblin,tinyMinotaur]
strongEnemies=[apokoliptian,orc,largeMinotaur]
bosses=[darkGod,warboss,minotaurKing]

deductHealthFromEnemy::Enemy->Int->Enemy
deductHealthFromEnemy enemy deduction=Enemy ((healthpoints enemy)-deduction) (enemyDamage enemy) (enemyHitChance enemy) (enemyName enemy)

enemyIsDead::Enemy->Bool
enemyIsDead enemy=(healthpoints enemy)<=0

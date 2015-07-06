module Weapons
(
  Weapon(..)
  ,fists
  ,weakWeapons
  ,mediocreWeapons
  ,strongWeapons
) where

data Weapon = Weapon {weaponDamage:: Int,weaponHitChance :: Float, weaponName :: String } deriving (Show)

bfg = Weapon 100 0.9 "BFG"
highlander = Weapon 100 0.9 "Highlander"
loic = Weapon 100 1 "Low Orbital Ion Cannon"

swordOfAwesome = Weapon 50 0.8 "Sword of Awesome"
rocketLauncher = Weapon 70 0.5 "Rocket Launcher"
revolver = Weapon 40 0.9 "Revolver"

blockSword = Weapon 30 0.9 "Block Sword"
pistol = Weapon 30 0.9 "Pistol"
bow=Weapon 40 0.5 "Bow"

fists = Weapon 20 1 "Fists"

weakWeapons=[blockSword,pistol,bow]
mediocreWeapons=[swordOfAwesome,rocketLauncher,revolver]
strongWeapons=[bfg,highlander,loic]

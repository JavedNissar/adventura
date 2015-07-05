import Weapons

module Hero
(
  Hero
  ,deductHealth
  ,changeWeapon
) where

data Hero = Hero{heroHealthPoints :: Int,currentWeapon :: Weapon} deriving (Show)

deductHealth::Hero->Int->Hero
deductHealth hero deduction=Hero ((heroHealthPoints hero)-deduction) (currentWeapon hero)

changeWeapon::Hero->Weapon->Hero
changeWeapon hero newWeapon=Hero (heroHealthPoints hero) newWeapon

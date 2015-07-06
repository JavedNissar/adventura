module Hero
(
  Hero(..)
  ,deductHealth
  ,changeWeapon
  ,heroIsDead
) where

import Weapons
data Hero = Hero{heroHealthPoints :: Int,currentWeapon :: Weapon} deriving (Show,Eq)

deductHealth::Hero->Int->Hero
deductHealth hero deduction=Hero ((heroHealthPoints hero)-deduction) (currentWeapon hero)

changeWeapon::Hero->Weapon->Hero
changeWeapon hero newWeapon=Hero (heroHealthPoints hero) newWeapon

heroIsDead::Hero->Bool
heroIsDead hero=(heroHealthPoints hero)<=0

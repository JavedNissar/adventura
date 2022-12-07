import System.Random
import Data.List
import Control.Monad
import Labyrinth
import Hero
import Enemies
import Utilities
import Weapons


generateWeaponHit:: Float->StdGen->Bool
generateWeaponHit weaponHitChance gen =
  let (r,_)=randomR (0,1) gen :: (Float,StdGen)
  in if r<weaponHitChance then True else False

fightEnemy::Hero->Enemy->[StdGen]->Maybe Hero
fightEnemy hero enemy gens
  |heroIsDead heroInAftermath = Nothing
  |enemyIsDead enemyInAftermath = Just heroInAftermath
  |otherwise = fightEnemy heroInAftermath enemyInAftermath gens
  where
      weaponOfHero=currentWeapon hero
      (index,_)=randomR (0,(length gens)-1) (gens!!0)::(Int,StdGen)
      genToUse=gens!!index
      heroHasHit=generateWeaponHit (weaponHitChance $ weaponOfHero) genToUse
      enemyInAftermath=if heroHasHit then deductHealthFromEnemy enemy (weaponDamage weaponOfHero) else enemy
      enemyHasHit=if (enemyIsDead enemy) then False else generateWeaponHit (enemyHitChance $ enemy) genToUse
      heroInAftermath=deductHealth hero (enemyDamage enemy)

fight::Hero->Path->[StdGen]->Maybe Hero
fight hero path gens=foldM (\changedHero enemy -> fightEnemy changedHero enemy gens) hero (enemies path) >>= (\changedHero -> Just $ changeWeapon changedHero (rewardWeapon path))

createDescriptionOfEnemy::(String,Int)->String
createDescriptionOfEnemy enemyTuple
  |(1==snd enemyTuple)=(show $ snd enemyTuple)++" "++(fst enemyTuple)
  |otherwise=(show $ snd enemyTuple)++" "++(fst enemyTuple)++"s"

play:: Hero->Int->Labyrinth->[StdGen]->IO ()
play hero index labyrinth gens = do
  if (length labyrinth)==index
    then putStrLn "Amazing! You finished Adventura."
    else do
      putStrLn "You see some new paths"
      path<-choosePath labyrinth index
      putStrLn "And the fight begins"
      let possiblyDeadHero=fight hero path gens
      if possiblyDeadHero==Nothing then putStrLn "Oh no! You died."
      else do
        putStrLn "You won!"
        let Just changedHero=possiblyDeadHero
        play changedHero (index+1) labyrinth gens

main = do
  gen<-getStdGen
  gen2<-newStdGen
  gen3<-newStdGen
  gen4<-newStdGen
  gen5<-newStdGen
  let gens=[gen,gen2,gen3,gen4,gen5]
      labyrinth=generateLabyrinth gens
      hero=Hero 100 fists
  play hero 0 labyrinth gens

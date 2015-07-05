import System.Random
import Data.List
import Control.Monad.Random
import Labyrinth
import Hero
import Enemies
import PathUtilities
import Weapons


generateCombatResult :: MonadRandom m => Rational -> m Bool
generateCombatResult weaponHitChance = fromList [(True,weaponHitChance),(False,1-weaponHitChance)]

numberOfEnemies::[Enemy]->Enemy->Int
numberOfEnemies enemiesList possibleEnemy=length $ filter (==possibleEnemy) enemiesList

convertBooleanToInteger :: Bool->Int
convertBooleanToInteger bool=if True==bool then 1 else 0

createDescriptionOfEnemy::(String,Int)->String
createDescriptionOfEnemy enemyTuple
  |(1==snd enemyTuple)=(show $ snd enemyTuple)++" "++(fst enemyTuple)
  |otherwise=(show $ snd enemyTuple)++" "++(fst enemyTuple)++"s"  

play:: Hero->Int->Labyrinth->IO ()
play hero index labyrinth = do
  if (length labyrinth)==index
    then putStrLn "Amazing! You finished Adventura."
    else do
      let path=choosePath labyrinth index
          changedHero=fight hero labyrinth labyrinthIndex pathIndex
      in do
        fight hero labyrinth labyrinthIndex pathIndex
        play hero (index+1) labyrinth


  main = do
    gen<-getStdGen
    gen2<-newStdGen
    gen3<-newStdGen
    gen4<-newStdGen
    gen5<-newStdGen
    let gens=[gen,gen2,gen3,gen4,gen5]
        labyrinth=generateLabyrinth gens
        hero=Hero 100 fists
    play hero 0 labyrinth

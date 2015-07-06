module Labyrinth
(
  Path(..)
  ,Labyrinth
  ,generateLabyrinth
) where

import System.Random
import Enemies
import Weapons

data Path = Path {enemies:: [Enemy],rewardWeapon :: Weapon} deriving (Show)
type Labyrinth = [[Path]]

generateListOfRandomInts :: RandomGen g => g -> Int -> (Int, Int) -> [Int]
generateListOfRandomInts generator number range = take number $ randomRs range generator :: [Int]

generatePathTemplate :: StdGen -> [Enemy] -> [Weapon] -> Int -> Path
generatePathTemplate generator enemies weapons maxEnemies=
  let (numEnemies,gen')=randomR (1,maxEnemies) generator :: (Int,StdGen)
      (weaponIndex,gen'')=randomR (1,(length weapons)-1) gen' :: (Int,StdGen)
      enemiesLength=length enemies
      randomInts=generateListOfRandomInts gen'' numEnemies (1,enemiesLength-1)
  in Path [enemies!!x|x<-randomInts] (weapons!!weaponIndex)

generateEasyPath :: StdGen -> Path
generateEasyPath generator=generatePathTemplate generator weakEnemies weakWeapons 5

generateModeratePath :: StdGen -> Path
generateModeratePath generator=generatePathTemplate generator strongEnemies mediocreWeapons 3

generateHardPath :: StdGen -> Path
generateHardPath generator=generatePathTemplate generator bosses strongWeapons 1

generatePath :: (Num a, Ord a) => a -> StdGen -> Path
generatePath difficultyLevel generator
  |difficultyLevel<=1 = generateEasyPath generator
  |difficultyLevel==2 = generateModeratePath generator
  |otherwise = generateHardPath generator

generateNumberOfPaths :: Int -> StdGen -> [Int]
generateNumberOfPaths n generator=take n $ randomRs (1,3) generator :: [Int]

generatePaths::[StdGen]->Int->[Int]->[[Path]]
generatePaths generators difficultyLevel listOfNumPaths=
  let randoms=take 200 $ randomRs (0,(length generators)-1) (generators!!0) :: [Int]
      randomGenerators=map (generators!!) randoms
      numPathsWithIndexes=zip listOfNumPaths [0..(length listOfNumPaths)-1]
  in [[generatePath difficultyLevel (randomGenerators!!((snd x)+y)) |y<-[0..(fst x)-1]]|x<-numPathsWithIndexes]

generateLabyrinth :: [StdGen] -> Labyrinth
generateLabyrinth gens =
  let numEasyPaths = generateNumberOfPaths 4 (gens!!0)
      easyPaths=generatePaths gens 1 numEasyPaths
      numMediumPaths=generateNumberOfPaths 5 (gens!!1)
      mediumPaths=generatePaths gens 2 numMediumPaths
      hardPaths=[[generateHardPath (gens!!2)]]
  in easyPaths++mediumPaths++hardPaths

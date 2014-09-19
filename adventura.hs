import System.Random
import Control.Monad.Random

data Weapon = Weapon {weaponDamage :: Int, weaponHitChance :: Float, weaponName :: String } deriving (Show)
data Enemy = Enemy {healthpoints :: Int,enemyDamage :: Int,enemyHitChance:: Float, rewardScore:: Int, enemyName :: String} deriving (Show)
data Path = Path {enemies:: [Enemy],rewardWeapon :: Weapon} deriving (Show)
type Labyrinth = [[Path]]

parademon = Enemy 10 10 0.5 10 "Parademon"
goblin = Enemy 20 20 0.3 10 "Goblin"
tinyMinotaur = Enemy 30 30 0.1 10 "Tiny Minotaur"

apokoliptian = Enemy 50 50 0.3 100 "Apokoliptian"
orc = Enemy 40 40 0.4 100 "Orc"
largeMinotaur = Enemy 30 30 0.5 100 "Large Minotaur"

darkGod = Enemy 100 70 0.5 10000 "Dark God"
warboss = Enemy 70 50 0.7 1000 "Orc Boss"
minotaurKing = Enemy 80 30 0.9 1000 "Minotaur King"

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

weakEnemies=[parademon,goblin,tinyMinotaur]
strongEnemies=[apokoliptian,orc,largeMinotaur]
bosses=[darkGod,warboss,minotaurKing]

weakWeapons=[blockSword,pistol,bow]
mediocreWeapons=[swordOfAwesome,rocketLauncher,revolver]
strongWeapons=[bfg,highlander,loic]

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

determineDifficulty :: (Num a1, Num a, Ord a1) => a1 -> a
determineDifficulty index
  |index<=3 = 1
  |index>3&&index<=8 = 2
  |otherwise = 3

generateNumPaths :: Int -> StdGen -> [Int]
generateNumPaths n generator=take n $ randomRs (1,3) generator :: [Int]

generatePaths::StdGen->Int->[Int]->[[Path]]
generatePaths generator difficultyLevel listOfNumPaths=[[generatePath difficultyLevel generator |y<-[1..x]]|x<-listOfNumPaths]

generateLabyrinth :: StdGen -> Labyrinth
generateLabyrinth generator =
  let numEasyPaths = generateNumPaths 4 generator
      easyPaths=generatePaths generator 1 numEasyPaths
      numMediumPaths=generateNumPaths 5 generator
      mediumPaths=generatePaths generator 2 numMediumPaths
      hardPaths=[[generateHardPath generator]]
  in easyPaths++mediumPaths++hardPaths

generateCombatResult :: MonadRandom m => Rational -> m Bool
generateCombatResult weaponHitChance = fromList [(True,weaponHitChance),(False,1-weaponHitChance)]

main = do
  gen<-getStdGen
  let labyrinth=generateLabyrinth gen
  play 0 [1..10]

play:: Int->[a]->IO ()
play index labyrinth = do
  if (length labyrinth)-1==index
    then putStrLn "Amazing! You finished Adventura"
    else do
      let newIndex=choosePath labyrinth index
      putStrLn $ show newIndex
      play newIndex labyrinth

choosePath labyrinth index = index+1

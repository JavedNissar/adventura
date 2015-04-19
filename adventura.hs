import System.Random
import Data.List
import Control.Monad.Random

data Weapon = Weapon {weaponDamage :: Int, weaponHitChance :: Float, weaponName :: String } deriving (Show)
data Enemy = Enemy {healthpoints :: Int,enemyDamage :: Int,enemyHitChance:: Float, rewardScore:: Int, enemyName :: String} deriving (Show,Eq)
data Hero = Hero{heroHealthPoints :: Int,currentWeapon :: Weapon} deriving (Show)
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

generateCombatResult :: MonadRandom m => Rational -> m Bool
generateCombatResult weaponHitChance = fromList [(True,weaponHitChance),(False,1-weaponHitChance)]

generateDescriptionOfPossiblePaths::Int->String
generateDescriptionOfPossiblePaths n
  |n==1 = "path"
  |2==n = "first path, or second path"
  |otherwise = " first path, second path, or third path"

describePathChosen::Integer->String
describePathChosen n
 |n==1 = "1st"
 |n==2 = "2nd"
 |otherwise = "3rd"

--numberOfEnemies::[Enemy]->Enemy->Int
numberOfEnemies enemiesList possibleEnemy=length $ filter (==possibleEnemy) enemiesList

convertBooleanToInteger :: Bool->Int
convertBooleanToInteger bool=if True==bool then 1 else 0

createDescriptionOfEnemy::(String,Int)->String
createDescriptionOfEnemy enemyTuple
  |(1==snd enemyTuple)=(show $ snd enemyTuple)++" "++(fst enemyTuple)
  |otherwise=(show $ snd enemyTuple)++" "++(fst enemyTuple)++"s"

descriptionOfEnemiesInPath :: [Path]->Int->String
descriptionOfEnemiesInPath paths index=
  let enemiesPresent=(enemies (paths!!index))
      numGoblins=((enemyName goblin),(numberOfEnemies enemiesPresent goblin))
      numParademons=((enemyName parademon),(numberOfEnemies enemiesPresent parademon))
      numTinyMinotaurs=((enemyName tinyMinotaur),(numberOfEnemies enemiesPresent tinyMinotaur))
      numApokoliptians=((enemyName apokoliptian),(numberOfEnemies enemiesPresent apokoliptian))
      numOrcs=((enemyName orc),(numberOfEnemies enemiesPresent orc))
      numLargeMinotaurs=((enemyName largeMinotaur),(numberOfEnemies enemiesPresent largeMinotaur))
      numDarkGods=((enemyName darkGod),(numberOfEnemies enemiesPresent darkGod))
      numMinotaurKings=((enemyName minotaurKing),(numberOfEnemies enemiesPresent minotaurKing))
      numWarbosses=((enemyName warboss),(numberOfEnemies enemiesPresent warboss))
      enemyNumbers=[numGoblins,numParademons,numTinyMinotaurs,numApokoliptians
                    ,numOrcs,numLargeMinotaurs,numDarkGods,numMinotaurKings,numWarbosses]
      filteredEnemyNumbers=filter (\x->(snd x)>0) enemyNumbers
      enemyListAsString=(intercalate ",") $ map createDescriptionOfEnemy (init filteredEnemyNumbers)
  in if (length enemyListAsString>0) then enemyListAsString++" and "++(createDescriptionOfEnemy (last filteredEnemyNumbers))
    else createDescriptionOfEnemy (last filteredEnemyNumbers)

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

play:: Hero->Int->Labyrinth->IO ()
play hero index labyrinth = do
  if (length labyrinth)==index
    then putStrLn "Amazing! You finished Adventura"
    else do
      choosePath labyrinth index
      putStrLn $ show (index+1)
      play hero (index+1) labyrinth

printOutPathDescriptions :: [Path] -> IO ()
printOutPathDescriptions paths= do
  if length paths>=3
    then do
      putStrLn $ "Path 1 has "++descriptionOfEnemiesInPath paths 0
      putStrLn $ "Path 2 has "++descriptionOfEnemiesInPath paths 1
      putStrLn $ "Path 3 has "++descriptionOfEnemiesInPath paths 2
    else do
      if length paths>=2
        then do
          putStrLn $ "Path 1 has "++descriptionOfEnemiesInPath paths 0
          putStrLn $ "Path 2 has "++descriptionOfEnemiesInPath paths 1
      else do
        putStrLn $ "The path has "++descriptionOfEnemiesInPath paths 0

describePathCount::Int->String
describePathCount n=intercalate ", " [show x|x<-[1..n]]

choosePath :: Labyrinth->Int->IO ()
choosePath labyrinth index = do
  let paths=labyrinth!!index
  putStrLn $ "You have "++(show $ length paths)++" to choose from"
  printOutPathDescriptions paths

  putStrLn ("Type "++(describePathCount (length paths))++" to go on the "++(generateDescriptionOfPossiblePaths (length paths)))
  numberAsString<-getLine
  let number=read numberAsString
  putStrLn ("You have entered the "++(describePathChosen number)++" path")

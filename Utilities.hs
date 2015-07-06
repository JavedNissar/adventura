module Utilities
(
  generateDescriptionOfPossiblePaths
  ,describePathChosen
  ,descriptionOfEnemiesInPath
  ,printOutPathDescriptions
  ,describePathCount
  ,choosePath
) where

import Labyrinth
import Enemies
import Data.List

numberOfPossibleEnemies::[Enemy]->Enemy->Int
numberOfPossibleEnemies enemiesList possibleEnemy=length $ filter (==possibleEnemy) enemiesList

generateDescriptionOfPossiblePaths::Int->String
generateDescriptionOfPossiblePaths n
  |n==1 = "path"
  |2==n = "first path, or second path"
  |otherwise = " first path, second path, or third path"

describePathChosen::Int->String
describePathChosen n
  |n==1 = "1st"
  |n==2 = "2nd"
  |otherwise = "3rd"

createDescriptionOfEnemy::(String,Int)->String
createDescriptionOfEnemy (enemyName,enemyCount)
  |(1==enemyCount)=(show enemyCount)++" "++(enemyName)
  |otherwise=(show enemyCount)++" "++enemyName++"s"

descriptionOfEnemiesInPath :: [Path]->Int->String
descriptionOfEnemiesInPath paths index=
  let enemiesPresent=(enemies (paths!!index))
      numGoblins=((enemyName goblin),(numberOfPossibleEnemies enemiesPresent goblin))
      numParademons=((enemyName parademon),(numberOfPossibleEnemies enemiesPresent parademon))
      numTinyMinotaurs=((enemyName tinyMinotaur),(numberOfPossibleEnemies enemiesPresent tinyMinotaur))
      numApokoliptians=((enemyName apokoliptian),(numberOfPossibleEnemies enemiesPresent apokoliptian))
      numOrcs=((enemyName orc),(numberOfPossibleEnemies enemiesPresent orc))
      numLargeMinotaurs=((enemyName largeMinotaur),(numberOfPossibleEnemies enemiesPresent largeMinotaur))
      numDarkGods=((enemyName darkGod),(numberOfPossibleEnemies enemiesPresent darkGod))
      numMinotaurKings=((enemyName minotaurKing),(numberOfPossibleEnemies enemiesPresent minotaurKing))
      numWarbosses=((enemyName warboss),(numberOfPossibleEnemies enemiesPresent warboss))
      enemyNumbers=[numGoblins,numParademons,numTinyMinotaurs,numApokoliptians
        ,numOrcs,numLargeMinotaurs,numDarkGods,numMinotaurKings,numWarbosses]
      filteredEnemyNumbers=filter (\x->(snd x)>0) enemyNumbers
      enemyListAsString=(intercalate ",") $ map createDescriptionOfEnemy (init filteredEnemyNumbers)
  in if (length enemyListAsString>0) then enemyListAsString++" and "++(createDescriptionOfEnemy (last filteredEnemyNumbers))
    else createDescriptionOfEnemy (last filteredEnemyNumbers)

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
describePathCount n
  |(n>1)=(intercalate ", " [show x|x<-[1..n-1]])++" or "++(show n)
  |otherwise="1"

choosePath :: Labyrinth->Int->IO Path
choosePath labyrinth index = do
    let paths=labyrinth!!index
    putStrLn $ "You have "++(show $ length paths)++" to choose from"
    printOutPathDescriptions paths

    putStrLn ("Type "++(describePathCount (length paths))++" to go on the "++(generateDescriptionOfPossiblePaths (length paths))++" respectively")
    numberAsString<-getLine
    let number=read numberAsString::Int
    putStrLn ("You have entered the "++(describePathChosen number)++" path")
    return (paths!!(number-1))

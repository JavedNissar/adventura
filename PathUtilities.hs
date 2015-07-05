import Labyrinth

module PathUtilities
(
  generateDescriptionOfPossiblePaths
  ,describePathChosen
  ,descriptionOfEnemiesInPath
  ,printOutPathDescriptions
  ,describePathCount
) where

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
  |(n>1)=(intercalate ", " [show x|x<-[1..n-1]])++" and "++(show n)
  |otherwise="1"

import System.Random

data Weapon = Weapon {weaponDamage :: Int, hitChance :: Float, weaponName :: String } deriving (Show)
data Enemy = Enemy {healthpoints :: Int,enemyDamage :: Int,hitChance:: Float, rewardScore:: Int, enemyName :: String} deriving (Show)
data Path = Path {enemies:: [Enemy],rewardWeapon :: Weapon}
type Labyrinth = [[Path]]

parademon = Enemy 10 10 0.5 "Parademon" 10
goblin = Enemy 20 20 0.3 "Goblin" 10
tinyMinotaur = Enemy 30 30 0.1 "Tiny Minotaur" 10

apokoliptian = Enemy 50 50 0.3 "Apokoliptian" 100
orc = Enemy 40 40 0.4 "Orc" 100
largeMinotaur = Enemy 30 30 0.5 "Large Minotaur" 100

darkGod = Enemy 100 70 0.5 "Dark God" 10000
warboss = Enemy 70 50 0.7 "Orc Boss" 1000
minotaurKing = Enemy 80 30 0.9 "Minotaur King" 1000

bfg = Weapon 100 0.9 "BFG"
highlander = Weapon 100 0.9 "Highlander"
loic = Weapon 100 1 "Low Orbital Ion Cannon"

swordOfAwesome = Weapon 50 0.8 "Sword of Awesome"
rocketLauncher = Weapon 70 0.5 "Rocket Launcher"
battleRifle = Weapon 60 0.7 "Battle Rifle"
revolver = Weapon 40 0.9 "Revolver"

blockSword = Weapon 30 0.9 "Block Sword"
pistol = Weapon 30 0.9 "Pistol"
fists = Weapon 20 1 "Fists"

--generatePath difficultyLevel generator=

--createLabyrinth generator len = [[]| x<-integers]
--  where integers=take len $ randomRs (1,3) generator :: [Int]

--main = do
--  gen <- getStdGen

module Constants where
import Types

tilesize = 20 :: Int
screenx = 800 :: Int
screeny = 520 :: Int
screenBpp = 32 :: Int
origPos = (320, 240) :: Pos
chance = 2 :: Int
--initx = [tilesize, 2 * tilesize .. 4*tilesize] :: [Int]
--inity = [tilesize, 2 * tilesize .. 3*tilesize] :: [Int]
initpos = (0, 0) :: Pos
rows = 3 :: Int
ncells = 4 :: Int
minl = 3 :: Int
maxl = 7 :: Int
minw = 3 :: Int
maxw = 8 :: Int
minmonsters = 3 :: Int
maxmonsters = 10 :: Int
--cells gen initpos rows cells maxlength [] [] []
--validx = [tilesize, 2 * tilesize .. screenx - 3 * tilesize] :: [Int]
--validy = [tilesize, 2 * tilesize .. screeny - 3 * tilesize] :: [Int]
--grid = [ (x, y) | x <- validx, y <- validy] :: [Pos]
--numPos = length grid
displacements = [0,tilesize..4*tilesize] :: [Int]
numDisp = length displacements

module Points where
import System.Random
import Types
import Constants
import Randomizers

line :: Int -> Int -> [Int]
line n length = [n, n + tilesize .. n + length * tilesize]

areaPoints :: Pos -> Int -> Int -> [Pos]
areaPoints (root_x, root_y) length width = [ (x, y) | x <- xs, y <- ys]
    where xs = line root_x width
          ys = line root_y length

--endToStart :: Int -> Int -> Int --takes an end x or y and a length or width and gives initial x or y
--endToStart n length = n - (length + 1) * tilesize

startToEnd :: Int -> Int -> Int 
startToEnd n length = n + (length + 1) * tilesize

roomEnd :: Room -> Pos
roomEnd ( (x, y), length, width) = (startToEnd x width, startToEnd y length) 

validLength :: Int -> Int -> Int -> Int
validLength n length bounds = if startToEnd n length >= bounds
                              then truncate $ abs $ fromIntegral exp / fromIntegral tilesize
                              else length                        
    where exp = bounds - 2 * tilesize - n

perimeterPoints :: Room -> [Pos]
perimeterPoints alcove = left ++ top ++ right ++ bottom 
    where
        left = leftPoints alcove
        top = topPoints alcove
        right = rightPoints alcove
        bottom = bottomPoints alcove

topPoints :: Room -> [Pos]
topPoints ( (x, y), _, width) = areaPoints (x + tilesize, y) 0 width

leftPoints :: Room -> [Pos]
leftPoints ( (x, y), length, _) = areaPoints (x, y) length 0

rightPoints :: Room -> [Pos]
rightPoints ( (x, y), length, width) = areaPoints (x + tilesize * (width + 1), y + tilesize) length 0

bottomPoints :: Room -> [Pos]
bottomPoints ( (x, y), length, width) = areaPoints (x, y + tilesize * (length + 1) ) 0 width

floorPoints :: Room -> [Pos]
floorPoints ( (x, y), length, width) = areaPoints (x + tilesize, y + tilesize) (length - 1) (width - 1)

topCave :: Room -> [Pos]
topCave ( (x, y), _, width) = topPoints ( (x + tilesize, y - tilesize), 0, width - 2)

leftCave :: Room -> [Pos]
leftCave ( (x, y), length, _) = leftPoints ( (x - tilesize, y + tilesize), length - 2, 0)

rightCave :: Room -> [Pos]
rightCave ( (x, y), length, width) = rightPoints ( (x + tilesize, y + tilesize), length - 2, width) 

bottomCave :: Room -> [Pos]
bottomCave ( (x, y), length, width) = bottomPoints ( (x + tilesize, y + tilesize), length, width - 2)

inBounds :: Int -> Int -> Int -> Bool
inBounds n start end
    | n >= start && n <= end = True
    | otherwise = False 

getDirection :: Direction -> Pos
getDirection direction = case direction of
                            North -> (0, -tilesize) 
                            South -> (0, tilesize)    
                            East -> (tilesize, 0)
                            West -> (-tilesize, 0)

doorPoints :: Cave -> [Pos]
doorPoints (_, start, end, startdoor, endoor) = doors
    where
        sdoor = start +^ (getDirection startdoor)     
        edoor = end +^ (getDirection endoor)
        doors = sdoor:edoor:[]

cavePos :: Cave -> [(Pos, Tile)]
cavePos (g, s, e, _, _) = [(caves, Floor) | caves <- points] 
    where points = cavePoints g s e []

doorPos :: Cave -> [(Pos, Tile)]
doorPos cave = [(d, Door) | d <- points]
    where points = doorPoints cave

wallPos :: Room -> [(Pos, Tile)]
wallPos room = [(w, Wall) | w <- wPos] 
    where wPos = leftPoints room ++ rightPoints room ++ topPoints room ++ bottomPoints room         

floorPos :: Room -> [(Pos, Tile)]
floorPos room = [(f, Floor) | f <- fPos]
    where fPos = floorPoints room

stairPos :: [Pos] -> [(Pos, Tile)]
stairPos stairs = (up, Stair True):(down, Stair False):[]
    where 
        up = head stairs
        down = last stairs

stairPoints :: StdGen -> [Room] -> ([Pos], StdGen)
stairPoints gen rooms = if head stairs /= last stairs
                        then (stairs, newGen)
                        else stairPoints newGen rooms
    where (stairs, newGen) = getTwoRandElem gen $ concat $ map floorPoints rooms 

monsterStartPos :: StdGen -> [Room] -> [Pos]
monsterStartPos gen rooms = monsters   
    where 
        (monsters, newGen) = getRandNumElem nGen num [] $ concat $ map floorPoints rooms    
        (num, nGen) = randomR (minmonsters, maxmonsters) gen :: (Int, StdGen)
        
cavePoints :: StdGen -> Pos -> Pos -> [Pos] -> [Pos]
cavePoints gen (x, y) (endx, endy) points
    | x > endx && toss || y == endy && x > endx = cavePoints newGen (x - tilesize, y) (endx, endy) ( (x, y):points)
    | x < endx && toss || y == endy && x < endx = cavePoints newGen (x + tilesize, y) (endx, endy) ( (x, y):points)
    | y > endy = cavePoints newGen (x, y - tilesize) (endx, endy) ( (x, y):points) 
    | y < endy = cavePoints newGen (x, y + tilesize) (endx, endy) ( (x, y):points)
    | otherwise = (x, y):points
    where (toss, newGen) = odds gen   
--first, increase (decrease if greater) startx until equal endx
--then, do same with starty and endy

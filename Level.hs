module Level where
import System.Random
import Control.Monad
import Data.List
import Graphics.UI.SDL.Image
import Graphics.UI.SDL as SDL
import qualified Data.Map as Map

import Constants
import Types
import Points
import Draw
import Randomizers

compareX :: Room -> Room -> Bool
compareX ( (firstx, _), _, _) ( (secondx, secondy), slen, swid)
    | firstx > secondx = True
    | otherwise = False
    --where (boundx, _) = roomEnd room1 

compareY :: Room -> Room -> Bool
compareY ( (_, firsty), _, _) ( (secondx, secondy), slen, swid)
    | firsty > secondy = True
    | otherwise = False 
    --where (_, boundy) = roomEnd room1

linkRooms :: StdGen -> Room -> Room -> Cave
linkRooms gen room1@( (x, y), length, width) room2@( (secondx, secondy), slen, swid)
    | not (compareX room1 room2) && (secondx > fboundx) = (gen, right1, left2, West, East)    
    | not (compareY room1 room2) && (secondx < fboundx) = (gen, bottom1, top2, North, South)
    | otherwise = (gen, bottom1, top2, North, South)    
    where 
            (fboundx, fboundy) = roomEnd room1
            (sboundx, sboundy) = roomEnd room2            
            (toss, _) = cointoss gen            
            (top1, gen1) = getRandomElem gen firsttop            
            (bottom1, gen2) = getRandomElem gen1 firstbottom             
            (right1, gen3) = getRandomElem gen2 firstright            
            (left1, gen4) = getRandomElem gen3 firstleft            
            (top2, gen5) = getRandomElem gen4 secondtop            
            (bottom2, gen6) = getRandomElem gen5 secondbottom            
            (left2, gen7) = getRandomElem gen6 secondleft           
            (right2, newGen) = getRandomElem gen7 secondright            
            firstbottom = bottomCave room1 
            firsttop = topCave room1             
            firstleft = leftCave room1            
            firstright = rightCave room1            
            secondtop = topCave room2
            secondright = rightCave room2            
            secondleft = leftCave room2
            secondbottom = bottomCave room2

cellRow :: StdGen -> Pos -> Int -> Int -> [Room] -> [Cave] -> ([Room], [Cave], Int, StdGen) --List of rooms and caves
cellRow gen (x, y) maxy numcells cells passages  
    | numcells > 1 && toss && vlength > 1 && vwidth > 1 && dispy <= 460 = if length cells > 0
        then cellRow newGen (nextx, y) newy (numcells - 1) (cell:cells) (passage:passages)
        else cellRow newGen (nextx, y) newy (numcells - 1) (cell:cells) passages
    | numcells > 1 && not toss = cellRow newGen (nextx, y) newy (numcells - 1) cells passages    
    | otherwise = if vlength <= 0 || vwidth <= 0 || dispx >= 700 || dispy >= 460 || length cells <= 1
              then (cells, passages, maxy, newGen)
              else (cell:cells, passage:passages, newy, newGen)    
    where 
        (len, width, nextGen1) = randDimensions gen        
        vlength = validLength dispy len screeny        
        vwidth = validLength dispx width screenx        
        (toss, _) = odds gen   
        cell = ( (dispx, dispy), vlength, vwidth)        
        passage = linkRooms gen (head cells) cell
        dispx = x + displacex        
        dispy = y + displacey       
        nextx = (startToEnd dispx width) + 2 * tilesize
        newy = max maxy (startToEnd dispy len)        
        (displacex, nextGen2) = randDisplace nextGen1
        (displacey, newGen) = randDisplace nextGen2         

cells :: StdGen -> Pos -> Int -> Int -> Int -> [Room] -> [Cave] -> [[Room]] -> ([Room], [Cave], [Pos]) 
cells gen (x, y) numrows numcells length rooms corridors previous
    | numrows > 1 = cells newGen (x, newy) (numrows - 1) numcells length (row ++ rooms) (newp ++ passages ++ corridors) (row:previous)
    | otherwise = (frooms, newp ++ passages ++ corridors, stairs)
    where
        (row, passages, maxy, newGen) = cellRow gen (x, y) 0 numcells [] []
        newp = linkRows gen previous row        
        newy = 2*tilesize + maxy
        frooms = row ++ rooms        
        (stairs, mGen) = stairPoints gen frooms
        mpos = monsterStartPos mGen frooms

linkRows :: StdGen -> [[Room]] -> [Room] -> [Cave]
linkRows _ [] _ = []
--linkRows _ (_:[]) _ = []
linkRows gen ([]:previous) current = linkRows gen previous current
linkRows gen (previous:_) current = zipWith (linkRooms gen) previous current
       
testing :: Pos -> Int -> Int -> Surface -> Surface -> Surface -> IO Bool
testing (x, y) length width wall floor screen = drawRoom ( (x, y), vlength, vwidth) wall floor screen
    where vlength = validLength y length screeny
          vwidth = validLength x width screenx

caveTest :: StdGen -> Surface -> Surface -> Surface -> Surface -> IO Bool
caveTest gen wall floor caveFloor screen = do
    drawRoom a wall floor screen    
    drawRoom b wall floor screen
    placeTiles cave caveFloor screen
    where 
            cave = cavePoints gen start end []
            (_, start, end, _, _) = linkRooms gen b a 
            a = ( (480, 40), 5, 6)            
            b = ( (20, 80), 7, 8)                
             
level :: StdGen -> Surface -> Surface -> Surface -> Surface -> Surface -> Surface -> Surface -> IO Bool
level gen wall floor dirt door stairsUp stairsDown screen = if length rooms >= 2
                                    then do
                                        drawCaves corridors dirt screen
                                        drawRooms rooms wall floor screen                                            
                                        drawDoors corridors door screen                                   
                                        drawStairs stairs stairsUp stairsDown screen                                    
                                        --placeTiles monsters monster screen                                    
                                    else level newGen wall floor dirt door stairsUp stairsDown screen  
    where (rooms, corridors, stairs) = cells gen initpos rows ncells maxl [] [] []
          (_, newGen) = random gen :: (Char, StdGen)

tiles :: [Room] -> [Cave] -> [Pos] -> Dungeon
tiles rooms corridors stairs = Map.fromList $ concat ( (map floorPos rooms) ++ (map cavePos corridors) ++ (map wallPos rooms) ++ (map doorPos corridors) ) ++ (stairPos stairs) 
{-  
main = withInit [InitEverything] $ do

    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Tiles test" []

    wall <- loadImage "wall.png" Nothing
    floor <- loadImage "cobblestone.png" Nothing
    dirt <- loadImage "dirt.png" Nothing    
    door <- loadImage "door.png" (Just (0xff, 0xff, 0xff) )    
    stairsUp <- loadImage "stairsUp.png" Nothing    
    stairsDown <- loadImage "stairsDown.png" Nothing    
    monster <- loadImage "monster.png" Nothing    
    gen <- getStdGen
    --cellRow (0, 0) 4 5 8 wall floor screen    
    --caveTest gen wall floor cave screen    
    level gen wall floor dirt door stairsUp stairsDown monster screen   
    --testing (650, 0) 10 8 wall floor screen    
    --room (20, 20) 2 3 wall floor screen    
    --drawRooms (cells gen (0, 0) 3 4 [] 5) wall floor screen    
    {-room (20, 20) 5 8 wall floor screen --cell 1
    room (220, 20) 5 8 wall floor screen --cell 2
    room (420, 20) 5 8 wall floor screen --cell 3
    room (20, 180) 5 8 wall floor screen --cell 4
    room (220, 180) 5 8 wall floor screen --cell 5
    room (420, 180) 5 8 wall floor screen --cell 6
    room (20, 340) 5 8 wall floor screen --cell 7
    room (220, 340) 5 8 wall floor screen --cell 8
    room (420, 340) 5 8 wall floor screen --cell 9   
    room (20, 20) 10 10 wall floor screen       
    room (300, 175) 12 6 wall floor screen
    room (400, 400) 2 4 wall floor screen
    room (0, 400) 4 6 wall floor screen 
    horizTiles (420, 25) 4 image screen 
    -}
    SDL.flip screen

    loop

    where 
        
        screenWidth = screenx
        screenHeight = screeny
        screenBpp = 32

        loop = do
            quit <- whileEvents
            unless quit loop

        whileEvents = do

            event <- pollEvent
            case event of
                Quit -> return True
                NoEvent -> return False
                _ -> whileEvents
-} 

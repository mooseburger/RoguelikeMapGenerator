module Draw where
import Control.Applicative
import System.Random
import Types
import Points

import Graphics.UI.SDL.Image
import Graphics.UI.SDL as SDL 

loadImage :: String -> Maybe RGB -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b) ) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
    where offset = Just Rect {rectX = x, rectY = y, rectW = 0, rectH = 0}

placeTiles :: [Pos] -> Surface -> Surface -> IO Bool
placeTiles [] tile screen = applySurface 0 0 tile screen Nothing
placeTiles ( (x, y):[]) tile screen = applySurface x y tile screen Nothing
placeTiles ( (x, y):coords) tile screen = do
    applySurface x y tile screen Nothing
    placeTiles coords tile screen

fillBlack :: Surface -> IO Bool    
fillBlack screen = do
    bgColor <- (mapRGB . surfaceGetPixelFormat) screen 0x00 0x00 0x00
    clipRect <- Just <$> (getClipRect screen)
    fillRect screen clipRect bgColor

drawRoom :: Room -> Surface -> Surface -> Surface -> IO Bool
drawRoom square wall floorTile screen = do
    placeTiles left wall screen --left   
    placeTiles top wall screen --top
    placeTiles right wall screen --right
    placeTiles bottom wall screen --bottom
    placeTiles floor floorTile screen --floor
    where 
        left = leftPoints square
        top = topPoints square
        right = rightPoints square
        bottom = bottomPoints square
        floor = floorPoints square

drawDoor :: Cave -> Surface -> Surface -> IO Bool
drawDoor (_, start, end, startdoor, endoor) door screen = placeTiles doors door screen
    where
        sdoor = start +^ (getDirection startdoor)     
        edoor = end +^ (getDirection endoor)
        doors = sdoor:edoor:[]

drawCave :: Cave -> Surface -> Surface -> IO Bool
drawCave (gen, start, end, _, _) dirt screen = do
    placeTiles points dirt screen
    where points = cavePoints gen start end []
                
drawRooms :: [Room] -> Surface -> Surface -> Surface -> IO Bool
drawRooms (alcove:[]) wall floor screen = drawRoom alcove wall floor screen 
drawRooms (alcove:rooms) wall floor screen = do
    drawRoom alcove wall floor screen
    drawRooms rooms wall floor screen

drawDoors :: [Cave] -> Surface -> Surface -> IO Bool
drawDoors [] door screen = placeTiles [] door screen
drawDoors (cave:[]) door screen = drawDoor cave door screen
drawDoors (cave:caves) door screen = do 
    drawDoor cave door screen
    drawDoors caves door screen

drawStairs :: [Pos] -> Surface -> Surface -> Surface -> IO Bool
drawStairs stairs stairsUp stairsDown screen = do
    placeTiles (up:[]) stairsUp screen
    placeTiles (down:[]) stairsDown screen
    where 
        up = head stairs
        down = last stairs

drawCaves :: [Cave] -> Surface -> Surface -> IO Bool
drawCaves [] dirt screen = placeTiles [] dirt screen 
drawCaves (cave:[]) dirt screen = drawCave cave dirt screen 
drawCaves (cave:caves) dirt screen = do
    drawCave cave dirt screen
    drawCaves caves dirt screen
    

module Move where
import Control.Monad.State
import qualified Data.Map as Map
import Constants
import Points
import Types 

move :: Dungeon -> Direction -> StateT Pos IO ()
move dungeon direction = do
    pos <- get
    put $ validPos pos (newPos pos direction) $ walkPos (newPos pos direction) dungeon
    return ()

upStair :: Dungeon -> StateT Pos IO ()
upStair dungeon = do
    pos <- get
    case walkPos pos dungeon of
        Stair True -> put (-2, -2)
        _ -> put pos
    return ()

downStair :: Dungeon -> StateT Pos IO ()
downStair dungeon = do
    pos <- get
    case walkPos pos dungeon of
        Stair False -> put (-1, -1)
        _ -> put pos
    return ()

newPos :: Pos -> Direction -> Pos
newPos pos dir = pos +^ (getDirection dir)

walkPos :: Pos -> Dungeon -> Tile
walkPos position dungeon = case Map.lookup position dungeon of
                                Nothing -> Wall
                                Just tile -> tile   

validPos :: Pos -> Pos -> Tile -> Pos
validPos (x, y) (nx, ny) ntile
    | ntile == Wall = (x, y)
    | otherwise = (nx, ny)

--moveMonster :: Pos -> Pos 


{-up :: Dungeon -> StateT Pos IO ()
up dungeon = do
    (x, y) <- get
    put $ validPos (x, y) (x, y - tilesize) $ walkPos (x, y - tilesize) dungeon
    return ()
    where next =  

down :: Dungeon -> StateT Pos IO ()
down dungeon = do
    (x, y) <- get
    put $ validPos (x, y) (x, y + tilesize) next
    return ()
    where next = walkPos (x, y + tilesize) dungeon

left :: Dungeon -> StateT Pos IO ()
left dungeon = do
    (x, y) <- get
    put $ validPos (x, y) (x - tilesize, y) dungeon
    return ()
    where next = walkPos (x - tilesize, y) dungeon

right :: Dungeon -> StateT Pos IO ()
right dungeon = do
    (x, y) <- get
    put $ validPos (x, y) (x + tilesize, y) dungeon
    return ()
    where next = walkPos (x + tilesize, y) dungeon
-}
sleep :: StateT Pos IO ()
sleep = do
    (x, y) <- get
    put (x, y)
    return ()

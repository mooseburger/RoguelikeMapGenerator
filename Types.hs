module Types where
import System.Random
import Data.Word
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Graphics.UI.SDL

data Tile = Wall | Floor | Door | Stair Bool deriving (Eq)
data Direction = North | South | East | West

data AppConfig = AppConfig {
    --generator :: StdGen,
    wall :: Surface,
    ground :: Surface,
    dirt :: Surface,
    door :: Surface,    
    stairsUp :: Surface,
    stairsDown :: Surface,    
    screen :: Surface,    
    critter :: Surface,    
    guy :: Surface,
    pos :: (Int, Int)
}

type Pos = (Int, Int)
type Room = (Pos, Int, Int)
type Cave = (StdGen, Pos, Pos, Direction, Direction)
type Dungeon = Map.Map Pos Tile
type RGB = (Word8, Word8, Word8)
type AppState = StateT Pos IO
type AppEnv = ReaderT AppConfig AppState

(x, y) +^ (ex, ey) = (x + ex, y + ey)

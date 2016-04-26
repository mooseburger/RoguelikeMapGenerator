module Main where
import Data.Word
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Random
import qualified Data.Map as Map
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image

import Constants
import Types
import Draw
import Move
import Level

runLoop :: AppConfig -> [StdGen] -> IO ( (), Pos)
runLoop config gens@(gen:gs) = runStateT (runReaderT (loop gens) config) pos
    where 
        (_, _, stairs) = cells gen initpos rows ncells maxl [] [] []    
        pos = head stairs
  
initEnv :: IO AppConfig
initEnv = do
    screen <- setVideoMode screenx screeny screenBpp [SWSurface]
    setCaption "Game Demo" "Movement Test"
    
    wall <- loadImage "/home/mooseburger/haskell/wall.png" Nothing
    ground <- loadImage "/home/mooseburger/haskell/cobblestone.png" Nothing
    dirt <- loadImage "/home/mooseburger/haskell/dirt.png" Nothing    
    door <- loadImage "/home/mooseburger/haskell/door.png" $ Just (0xff, 0xff, 0xff) 
    monster <- loadImage "/home/mooseburger/haskell/monster.png" $ Just (0xf5, 0xde, 0xb3)    
    guy <- loadImage "/home/mooseburger/haskell/guy.png" $ Just (0x00, 0xff, 0xff)    
    stairsUp <- loadImage "/home/mooseburger/haskell/stairsUp.png" Nothing    
    stairsDown <- loadImage "/home/mooseburger/haskell/stairsDown.png" Nothing
    --gen <- getStdGen
    
    
    return $ AppConfig wall ground dirt door stairsUp stairsDown screen monster guy origPos
    

loop :: [StdGen] -> AppEnv ()
loop gens@(gen:gs) = do
    quit <- whileEvents $ \event -> do
        case event of
            (KeyDown (Keysym key _ _) ) -> do
                case key of
                    SDLK_w -> ReaderT (\_ -> move dungeon North)
                    SDLK_s -> ReaderT (\_ -> move dungeon South)
                    SDLK_a -> ReaderT (\_ -> move dungeon West)
                    SDLK_d -> ReaderT (\_ -> move dungeon East)
                    SDLK_PERIOD -> ReaderT (\_ -> downStair dungeon)                    
                    SDLK_COMMA -> ReaderT (\_ -> upStair dungeon)
                    _ -> ReaderT (\_ -> sleep)
            _ -> return ()
    
   
    wall <- liftM wall ask
    ground <- liftM ground ask
    dirt <- liftM dirt ask
    door <- liftM door ask    
    stairsUp <- liftM stairsUp ask
    stairsDown <- liftM stairsDown ask    
    screen <- liftM screen ask
    --monster <- liftM critter ask    
    guy <- liftM guy ask
    (x, y) <- get
    fillBlack' screen    
    case (x, y) of
        (-1, -1) -> getFloor ngen (ngen:gens) (nx, ny) wall ground dirt door guy stairsUp stairsDown screen quit

        (-2, -2) -> getFloor pgen gs (px, py) wall ground dirt door guy stairsUp stairsDown screen quit 
        
        _ -> getFloor gen gens (x, y) wall ground dirt door guy stairsUp stairsDown screen quit
                
    where 
        (rooms, corridors, s) = cells gen initpos rows ncells maxl [] [] []
        dungeon = tiles rooms corridors s        
        applySurface' x y src dst clip = liftIO (applySurface x y src dst clip)
        fillBlack' screen = liftIO (fillBlack screen) 
        level' gen wall floor dirt door stairsUp stairsDown screen = liftIO (level gen wall floor dirt door stairsUp stairsDown screen)
        (_, _, stairs) = cells ngen initpos rows ncells maxl [] [] []    
        (_, _, pstairs) = cells pgen initpos rows ncells maxl [] [] []        
        (nx, ny) = head stairs
        (px, py) = last pstairs        
        (_, ngen) = random gen :: (Char, StdGen)
        pgen = head gs        
        getFloor g levels (x, y) wall ground dirt door guy stairsUp stairsDown screen quit = do 
        
            level' g wall ground dirt door stairsUp stairsDown screen    
            applySurface' x y guy screen Nothing
            return ()
            liftIO $ SDL.flip screen                
            put (x, y)                
            unless quit $ loop levels                    
  
whileEvents :: MonadIO m => (Event -> m () ) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _ -> do
            act event
            whileEvents act

main = withInit [InitEverything] $ do

    env <- initEnv
    gen <- getStdGen    
    runLoop env $ gen:[]
            

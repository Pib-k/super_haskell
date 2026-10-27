module Main (main) where

import Game.Engine (stepPure)
import Game.Input (handleInputPure, normalize)
import Game.Levels
import Game.State (AppMode (..), GameState (..))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), playIO)
import Graphics.Render (viewPure)
import Storage.Save (applySave, loadSave, saveFile, saveProgress)
import System.Random (getStdGen)

initialState :: IO GameState
initialState = do
    gen <- getStdGen
    pure
        GameState
            { appMode = MainMenu
            , currentLevel = thePrelude
            , availableLevels = ["The Prelude", "The Monad Maze", "The IO Underground"]
            , levelIndex = 0
            , isPaused = False
            , globalScore = 0
            , rngSeed = 0
            , rngState = gen
            , isDebugMode = False
            }

viewWorld :: GameState -> IO Picture
viewWorld = pure . viewPure

stepWorld :: Float -> GameState -> IO GameState
stepWorld dt = pure . stepPure dt

handleInput :: Event -> GameState -> IO GameState
handleInput ev@(EventKey key Down _ _) gs
    | k == Char 'k' = saveProgress saveFile gs >> pure gs
    | k == Char 'l' = maybe gs (`applySave` gs) <$> loadSave saveFile
    | otherwise = pure (handleInputPure ev gs)
  where
    k = normalize key
handleInput ev gs = pure (handleInputPure ev gs)

main :: IO ()
main = do
    state0 <- initialState
    playIO
        (InWindow "Super Haskell" (800, 600) (100, 100))
        black
        60
        state0
        viewWorld
        handleInput
        stepWorld

module Main (main) where

import Core.Types
import Game.Engine (stepPure)
import Game.Input (handleInputPure, normalize)
import Game.Levels
import Game.State (AppMode (..), GameState (..))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), playIO)
import Graphics.Render (viewPure)
import Storage.Save
import System.Random (getStdGen, uniform)

initialState :: IO GameState
initialState = do
    gen <- getStdGen
    let (seed, gen') = uniform gen
    pure
        GameState
            { appMode = MainMenu
            , levelState = testLevel
            , level = minBound
            , pauseSelection = Resume
            , globalScore = 0
            , highScore = 0
            , rngSeed = seed
            , rngState = gen'
            , isDebugMode = False
            , menuIndex = 0
            , windowDim = (800, 600)
            }

viewWorld :: Assets -> GameState -> IO Picture
viewWorld assets = pure . viewPure assets

stepWorld :: Float -> GameState -> IO GameState
stepWorld dt gs = pure $ stepPure dt gs

handleInput :: Event -> GameState -> IO GameState
handleInput ev@(EventKey key Down _ _) gs
    | k == Char 'k' = saveProgress saveFile gs >> pure gs
    | k == Char 'l' = maybe gs id <$> loadSave saveFile
    | otherwise = pure (handleInputPure ev gs)
  where
    k = normalize key
handleInput ev gs = pure (handleInputPure ev gs)

loadAssets :: IO Assets
loadAssets = do
    playerSprite <- loadBMP "assets/playerSprite.bmp"
    pure Assets{playerImg = playerSprite, typeMismatchImg = Nothing, spaceLeakImg = Nothing}

main :: IO ()
main = do
    state0 <- initialState
    assets <- loadAssets
    playIO
        (InWindow "Super Haskell" (800, 600) (100, 100))
        black
        60
        state0
        (viewWorld assets)
        handleInput
        stepWorld

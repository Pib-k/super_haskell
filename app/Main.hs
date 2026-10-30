module Main (main) where

import Core.Types
import Foreign.C (withCString)
import GHC.ForeignPtr (newForeignPtr_)
import Game.Engine (stepPure)
import Game.Input (handleInputPure, normalize)
import Game.Levels
import Game.State (AppMode (..), GameState (..))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), playIO)
import Graphics.Render (viewPure)
import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Raw.Mixer as RawMixer
import Storage.Save
import System.Random (getStdGen, uniform)
import Unsafe.Coerce (unsafeCoerce)

initialState :: IO GameState
initialState = do
    gen <- getStdGen
    let (seed, gen') = uniform gen
    pure
        GameState
            { appMode = MainMenu
            , currentLevel = testLevel
            , availableLevels = ["The Prelude", "The Monad Maze", "The IO Underground"]
            , levelIndex = 0
            , isPaused = False
            , globalScore = 0
            , highScore = 0
            , rngSeed = seed
            , rngState = gen'
            , isDebugMode = False
            }
initMusic :: FilePath -> IO Mixer.Music
initMusic path = do
    SDL.initialize [SDL.InitAudio]
    Mixer.openAudio Mixer.defaultAudio 4096
    -- SDL is a C package. We need a foreignPtr in order to prevent
    -- garbage collection (and thus the stopping of the music).
    rawPtr <- withCString path RawMixer.loadMUS
    _ <- RawMixer.playMusic rawPtr (-1)
    fptr <- newForeignPtr_ rawPtr
    pure (unsafeCoerce fptr)

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
    typeMismatchSprite <- loadBMP "assets/typeMismatch.bmp"
    music <- initMusic "assets/game_song.wav"
    pure Assets{playerImg = playerSprite, typeMismatchImg = Just typeMismatchSprite, spaceLeakImg = Nothing, gameMusic = music}

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

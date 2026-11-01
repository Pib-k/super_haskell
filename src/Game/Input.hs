module Game.Input where

import Data.Char (toLower)
import Entities.Player (playerKey)
import Game.State
import Game.Levels (levelFor)
import Core.Constants
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), SpecialKey (..))

handleInputPure :: Event -> GameState -> GameState
handleInputPure (EventKey key keyState _ _) gs =
    case appMode gs of
        MainMenu -> menuKey k keyState gs
        LevelSelection -> selectionKey k keyState gs
        Playing -> playingKey k keyState gs
        LevelComplete -> completeKey k keyState gs
        GameOver -> gameOverKey k keyState gs
        Controls -> controlsKey k keyState gs
  where
    k = normalize key
handleInputPure _ gs = gs

normalize :: Key -> Key
normalize (Char c) = Char (toLower c)
normalize k = k

menuKey :: Key -> KeyState -> GameState -> GameState
menuKey (Char 'w') Down gs = moveMenu (-1) gs
menuKey (Char 's') Down gs = moveMenu 1 gs
menuKey (SpecialKey KeyUp) Down gs = moveMenu (-1) gs
menuKey (SpecialKey KeyDown) Down gs = moveMenu 1 gs
menuKey (SpecialKey KeyEnter) Down gs = case menuIndex gs of
    0 -> gs{appMode = Playing, levelIndex = 0, currentLevel = levelFor 0}
    1 -> gs{appMode = LevelSelection}
    _ -> gs{appMode = Controls}
menuKey _ _ gs = gs

moveMenu :: Int -> GameState -> GameState
moveMenu d gs = gs{menuIndex = (menuIndex gs + d) `mod` length menuItems}

selectionKey :: Key -> KeyState -> GameState -> GameState
selectionKey key Down gs = case key of
    Char 'w' -> moveLevel (-1)
    Char 's' -> moveLevel 1
    Char 'a' -> moveLevel (-1)
    Char 'd' -> moveLevel 1
    SpecialKey KeyUp -> moveLevel (-1)
    SpecialKey KeyDown -> moveLevel 1
    SpecialKey KeyEnter -> gs{appMode = Playing, currentLevel = levelFor (levelIndex gs)}
    Char 'm' -> gs{appMode = MainMenu}
    _ -> gs
  where
    lastIdx = length (availableLevels gs) - 1
    moveLevel d = gs{levelIndex = (levelIndex gs + d) `mod` (lastIdx + 1)}
selectionKey _ _ gs = gs

playingKey :: Key -> KeyState -> GameState -> GameState
playingKey (Char 'p') Down gs = gs{isPaused = not (isPaused gs)}
playingKey (Char 'b') Down gs = gs{isDebugMode = not (isDebugMode gs)}
playingKey key keyState gs
    | isPaused gs = gs
    | otherwise = modifyPlayer (playerKey key keyState) gs


completeKey :: Key -> KeyState -> GameState -> GameState
completeKey (SpecialKey KeyEnter) Down gs =
    gs{appMode = LevelSelection, levelIndex = min (length (availableLevels gs) - 1) (levelIndex gs + 1)}
completeKey _ _ gs = gs

gameOverKey :: Key -> KeyState -> GameState -> GameState
gameOverKey (SpecialKey KeyEnter) Down gs =
    gs{appMode = Playing, currentLevel = levelFor (levelIndex gs)}
gameOverKey (Char 'm') Down gs = gs{appMode = MainMenu}
gameOverKey _ _ gs = gs

controlsKey :: Key -> KeyState -> GameState -> GameState
controlsKey (SpecialKey KeyEnter) Down gs = gs{appMode = MainMenu}
controlsKey (Char 'm') Down gs = gs{appMode = MainMenu}
controlsKey _ _ gs = gs
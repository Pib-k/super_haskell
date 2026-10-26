module Game.Input where

import Data.Char (toLower)
import Entities.Player (playerKey)
import Game.State
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), SpecialKey (..))

handleInputPure :: Event -> GameState -> GameState
handleInputPure (EventKey key keyState _ _) gs =
    case appMode gs of
        MainMenu -> menuKey k keyState gs
        LevelSelection -> selectionKey k keyState gs
        Playing -> playingKey k keyState gs
  where
    k = normalize key
handleInputPure _ gs = gs

normalize :: Key -> Key
normalize (Char c) = Char (toLower c)
normalize k = k

menuKey :: Key -> KeyState -> GameState -> GameState
menuKey (SpecialKey KeyEnter) Down gs = gs{appMode = LevelSelection}
menuKey _ _ gs = gs

selectionKey :: Key -> KeyState -> GameState -> GameState
selectionKey key Down gs = case key of
    Char 'a' -> gs{levelIndex = max 0 (levelIndex gs - 1)}
    Char 'd' -> gs{levelIndex = min lastIdx (levelIndex gs + 1)}
    SpecialKey KeyEnter -> gs{appMode = Playing}
    _ -> gs
  where
    lastIdx = length (availableLevels gs) - 1
selectionKey _ _ gs = gs

playingKey :: Key -> KeyState -> GameState -> GameState
playingKey (Char 'p') Down gs = gs{isPaused = not (isPaused gs)}
playingKey (Char 'b') Down gs = gs{isDebugMode = not (isDebugMode gs)}
playingKey key keyState gs
    | isPaused gs = gs
    | otherwise = modifyPlayer (playerKey key keyState) gs

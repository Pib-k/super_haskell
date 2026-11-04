module Game.Input where

import Core.Constants
import Core.Types (Levels (..), PauseOption (..))
import Data.Char (toLower)
import Entities.Player (playerKey)
import Game.Levels (levelFor)
import Game.State
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), SpecialKey (..))

handleInputPure :: Event -> GameState -> GameState
handleInputPure (EventResize (w, h)) gs = gs{windowDim = (fromIntegral w, fromIntegral h)}
handleInputPure (EventKey key keyState _ _) gs =
    case appMode gs of
        MainMenu -> menuKey k keyState gs
        LevelSelection -> selectionKey k keyState gs
        Playing -> playingKey k keyState gs
        Paused opt -> pauseKey opt k keyState gs
        LevelComplete -> completeKey k keyState gs
        GameOver _ -> gameOverKey k keyState gs
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
    0 -> gs{appMode = Playing, levelState = levelFor Prelude}
    1 -> gs{appMode = LevelSelection}
    _ -> gs{appMode = Controls}
menuKey _ _ gs = gs

moveMenu :: Int -> GameState -> GameState
moveMenu d gs = gs{menuIndex = (menuIndex gs + d) `mod` length menuItems}

selectionKey :: Key -> KeyState -> GameState -> GameState
selectionKey key Down gs = case key of
    Char 'w' -> prev
    Char 's' -> next
    Char 'a' -> prev
    Char 'd' -> next
    SpecialKey KeyUp -> prev
    SpecialKey KeyDown -> next
    SpecialKey KeyEnter -> gs{appMode = Playing, levelState = levelFor (level gs)}
    Char 'm' -> gs{appMode = MainMenu}
    _ -> gs
  where
    prev = gs{level = previousLevel (level gs)}
    next = gs{level = nextLevel (level gs)}
selectionKey _ _ gs = gs

nextLevel :: Levels -> Levels
nextLevel l
    | l == maxBound = minBound
    | otherwise = succ l

previousLevel :: Levels -> Levels
previousLevel l
    | l == minBound = maxBound
    | otherwise = pred l

pauseKey :: PauseOption -> Key -> KeyState -> GameState -> GameState
pauseKey opt key Down gs = case key of
    Char 'w' -> gs{appMode = Paused (togglePauseOption opt)}
    Char 's' -> gs{appMode = Paused (togglePauseOption opt)}
    SpecialKey KeyUp -> gs{appMode = Paused (togglePauseOption opt)}
    SpecialKey KeyDown -> gs{appMode = Paused (togglePauseOption opt)}
    SpecialKey KeyEnter -> case opt of
        Resume -> gs{appMode = Playing}
        ReturnMainMenu -> gs{appMode = MainMenu}
    Char 'p' -> gs{appMode = Playing}
    _ -> gs
pauseKey _ _ _ gs = gs

togglePauseOption :: PauseOption -> PauseOption
togglePauseOption Resume = ReturnMainMenu
togglePauseOption ReturnMainMenu = Resume

playingKey :: Key -> KeyState -> GameState -> GameState
playingKey (Char 'p') Down gs = gs{appMode = Paused Resume}
playingKey (Char 'b') Down gs = gs{isDebugMode = not (isDebugMode gs)}
playingKey key keyState gs
    | appMode gs == Playing = modifyPlayer (playerKey key keyState) gs
    | otherwise = gs

completeKey :: Key -> KeyState -> GameState -> GameState
completeKey (SpecialKey KeyEnter) Down gs =
    gs{appMode = LevelSelection, level = succ (level gs)}
completeKey _ _ gs = gs

gameOverKey :: Key -> KeyState -> GameState -> GameState
gameOverKey (SpecialKey KeyEnter) Down gs =
    gs{appMode = Playing, levelState = levelFor (level gs)}
gameOverKey (Char 'm') Down gs = gs{appMode = MainMenu}
gameOverKey _ _ gs = gs

controlsKey :: Key -> KeyState -> GameState -> GameState
controlsKey (SpecialKey KeyEnter) Down gs = gs{appMode = MainMenu}
controlsKey (Char 'm') Down gs = gs{appMode = MainMenu}
controlsKey _ _ gs = gs

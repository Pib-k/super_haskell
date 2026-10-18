module IO where

import Engine
import GHC.Event
import Graphics.Gloss (Picture)
import Types

viewWorld :: GameState -> IO Picture
viewWorld = pure . viewPure

stepWorld :: Float -> GameState -> IO GameState
stepWorld dt = pure . stepPure dt

handleInput :: Event -> GameState -> IO GameState
handleInput ev = pure . handleInputPure ev

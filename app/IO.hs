module IO where

import Engine
import Graphics.Gloss (Picture)
import Types
import Graphics.Gloss.Interface.IO.Game (Event)
import Render

viewWorld :: GameState -> IO Picture
viewWorld = pure . viewPure

stepWorld :: Float -> GameState -> IO GameState
stepWorld dt = pure . stepPure dt

handleInput :: Event -> GameState -> IO GameState
handleInput ev = pure . handleInputPure ev

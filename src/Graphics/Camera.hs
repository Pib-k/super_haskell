module Graphics.Camera where

import Core.Constants (screenHalfWidth)
import Entities.Player (Player (..))
import Game.State (LevelState (..))
import Graphics.Gloss (Picture, translate)

cameraX :: LevelState -> Float
cameraX lvl = max (negate limit) (min limit px)
  where
    px = fst (playerPos (player lvl))
    limit = max 0 (fst (bounds lvl) / 2 - screenHalfWidth)

withCamera :: LevelState -> Picture -> Picture
withCamera lvl = translate (negate (cameraX lvl)) 0
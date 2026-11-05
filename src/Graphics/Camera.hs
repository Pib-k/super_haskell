module Graphics.Camera where

import Core.Constants (gameZoom)
import Entities.Player (Player (..))
import Game.State (LevelState (..))
import Graphics.Gloss (Picture, translate)
import Graphics.Gloss.Data.Picture (scale)

-- | Calculates the offset for the camera to follow the player, clamped by level boundaries.
cameraPos :: Float -> LevelState -> (Float, Float)
cameraPos halfW lvl = (camX, camY)
  where
    (px, py) = playerPos (player lvl)
    visibleHalfW = halfW / gameZoom
    limitX = max 0 (fst (bounds lvl) / 2 - visibleHalfW)
    camX = max (negate limitX) (min limitX px)
    camY = max (-130) (py + 30)

withCamera :: Float -> LevelState -> Picture -> Picture
withCamera halfW lvl world =
    scale gameZoom gameZoom $
        translate (negate camX) (negate camY) world
  where
    (camX, camY) = cameraPos halfW lvl

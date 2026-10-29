module Graphics.RenderAnim () where

import Core.Classes
import Core.Constants
import Entities.Enemy ()
import Entities.Player ()
import Entities.Terrain ()
import Entities.Collectible (CollectibleType (..), drawCollectibleType)
import Game.State
import Graphics.Gloss

instance Renderable Animation where
    render _dbg a =
        uncurry translate (animPos a) $
            case animType a of
                DefeatExplosion -> explosionPicture a
                MainDoFountain -> blank

explosionPicture :: Animation -> Picture
explosionPicture a = pictures
    [ translate (vx * t) (vy * t + 0.5 * gravity * t * t) $ drawCollectibleType kind (20, 20) | (kind, vx, vy) <- particles ]
  where
    t = animTimer a

    particles =
        [ (FloatingTrue, -65, 220)
        , (SemiGroup,    -35, 260)
        , (FloatingTrue, -15, 190)
        , (SemiGroup,      5, 240)
        , (FloatingTrue,  25, 270)
        , (SemiGroup,     45, 210)
        , (FloatingTrue,  65, 250)
        , (SemiGroup,     80, 180)
        ]
module Game.Animation where

import Core.Classes
import Core.Constants
import Core.Types
import Entities.Collectible
import Entities.Enemy
import Graphics.Gloss

data AnimationType = DefeatExplosion | MainDoFountain
    deriving (Eq, Show)

data Animation = Animation
    { animPos :: Position
    , animType :: AnimationType
    , animTimer :: Float
    , animDuration :: Float
    , frameIndex :: Int
    }
    deriving (Eq, Show)

explosion :: Enemy -> Animation
explosion e =
    Animation
        { animPos = getPos e
        , animType = DefeatExplosion
        , animTimer = 0
        , animDuration = explosionDuration
        , frameIndex = 0
        }

instance Renderable Animation where
    render _dbg a =
        uncurry translate (animPos a) $
            case animType a of
                DefeatExplosion -> explosionPicture a
                MainDoFountain -> blank

explosionPicture :: Animation -> Picture
explosionPicture a =
    pictures
        [translate (vx * t) (vy * t + 0.5 * gravity * t * t) $ drawCollectibleType kind (20, 20) | (kind, vx, vy) <- particles]
  where
    t = animTimer a

    particles =
        [ (FloatingTrue, -65, 220)
        , (SemiGroup, -35, 260)
        , (FloatingTrue, -15, 190)
        , (SemiGroup, 5, 240)
        , (FloatingTrue, 25, 270)
        , (SemiGroup, 45, 210)
        , (FloatingTrue, 65, 250)
        , (SemiGroup, 80, 180)
        ]

updateAnimation :: Float -> [Animation] -> [Animation]
updateAnimation dt = filter alive . map tick
  where
    tick a =
        let t = animTimer a + dt
         in a{animTimer = t, frameIndex = frameFor a t}
    alive a = animTimer a < animDuration a
    frameFor a t = min (explosionFrames - 1) (floor (t / animDuration a * fromIntegral explosionFrames))

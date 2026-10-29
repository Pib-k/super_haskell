module Game.Animation (updateAnimation, explosion) where

import Core.Classes
import Core.Constants
import Entities.Enemy
import Game.State

explosion :: Enemy -> Animation
explosion e = Animation
    {
        animPos = getPos e
    ,   animType = DefeatExplosion
    ,   animTimer = 0
    ,   animDuration = explosionDuration
    ,   frameIndex = 0
    }

updateAnimation :: Float -> [Animation] -> [Animation]
updateAnimation dt = filter alive . map tick
    where
        tick a =
            let t = animTimer a + dt
            in a{animTimer = t, frameIndex = frameFor a t}
        alive a = animTimer a < animDuration a
        frameFor a t = min (explosionFrames - 1) (floor (t / animDuration a * fromIntegral explosionFrames))


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Animation where

import Core.Classes
import Core.Constants
import Core.Types
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Entities.Enemy
import GHC.Generics
import Graphics.Gloss
import System.Random

data AnimationType = DefeatExplosion | MainDoFountain
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Tracks the state and lifecycle of an active animation.
data Animation = Animation
    { animPos :: Position
    , animType :: AnimationType
    , animTimer :: Float
    , animDuration :: Float
    , frameIndex :: Int
    , seed :: Int
    , animScale :: Float
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Spawns an explosion animation scaled to the defeated enemy's size.
explosion :: Enemy -> Animation
explosion e =
    Animation
        { animPos = getPos e
        , animType = DefeatExplosion
        , animTimer = 0
        , animDuration = explosionDuration
        , frameIndex = 0
        , seed = 0
        , animScale = max 0.5 (max ew eh / standardSize)
        }
  where
    (ew, eh) = getBox e
    standardSize = 30.0

-- | Single particle shot by the main-do fountain.
data FountainParticle = FP
    { fpSymbol :: String
    , fpLaunchTime :: Float
    , fpColor :: Color
    , fpVx :: Float
    , fpVy :: Float
    }

mainDoFountain :: Position -> Int -> Animation
mainDoFountain pos s =
    Animation
        { animPos = pos
        , animType = MainDoFountain
        , animTimer = 0
        , animDuration = mainDoAnimationDuration
        , frameIndex = 0
        , seed = s
        , animScale = 1.0
        }

instance Renderable Animation where
    render _dbg a =
        uncurry translate (animPos a) $
            case animType a of
                DefeatExplosion -> explosionPicture a
                MainDoFountain -> fountainPicture a

explosionPicture :: Animation -> Picture
explosionPicture a =
    scale sc sc $ pictures (shockwave : sparks)
  where
    sc = animScale a
    t = animTimer a
    progress = t / animDuration a
    fade = max 0.0 (1.0 - progress)
    shockwave =
        color (makeColor 1.0 0.9 0.4 (fade * 0.8)) $
            thickCircle (explosionRadius * progress) 2.5

    sparks =
        [ translate (vx * t) (vy * t + 0.5 * gravity * t * t)
            $ color (makeColor 1.0 cVal 0.1 fade)
            $ circleSolid (particleSize * fade)
        | (vx, vy, cVal) <-
            [ (-120, 140, 0.8)
            , (120, 140, 0.8)
            , (-150, 60, 0.5)
            , (150, 60, 0.5)
            , (-90, 200, 0.9)
            , (90, 200, 0.9)
            , (-50, 110, 0.4)
            , (50, 110, 0.4)
            ]
        ]

fountainPicture :: Animation -> Picture
fountainPicture a = case animType a of
    MainDoFountain ->
        pictures
            [ if tLocal >= 0 && tLocal < 1.2
                then
                    translate (fpVx p * tLocal) (fpVy p * tLocal + 0.5 * gravity * tLocal * tLocal)
                        $ color (fpColor p)
                        $ scale 0.1 0.1
                        $ text (fpSymbol p)
                else Blank
            | p <- generateParticles $ seed a
            , let tLocal = animTimer a - fpLaunchTime p
            ]
    _ -> Blank

-- | Generates a batch of fountain particles using a seeded RNG.
generateParticles :: Int -> [FountainParticle]
generateParticles s =
    fst $
        foldl
            ( \(acc, g) i ->
                let (p, g') = makeParticle i g
                 in (p : acc, g')
            )
            ([], gen0)
            [0 .. 29]
  where
    gen0 = mkStdGen s

makeParticle :: Int -> StdGen -> (FountainParticle, StdGen)
makeParticle i gen =
    let (idx, g1) = uniformR (0, length pool - 1) gen
        (vx, g2) = uniformR (-90.0, 90.0) g1
        (vy, g3) = uniformR (380.0, 560.0) g2
        (jit, g4) = uniformR (0.0, 0.1) g3
        (cIdx, g5) = uniformR (0, length colorPool - 1) g4
        launchTime = (fromIntegral i / 30.0) * 3.5 + jit
     in (FP (pool !! idx) launchTime (colorPool !! cIdx) vx vy, g5)
  where
    colorPool = [yellow, cyan, magenta, green, orange, rose, violet, aquamarine, white]
    pool = ["map", "filter", "foldr", "(>>=)", "(<$>)", "pure", "id", "(.)", "join", "fix", "λ"]

-- | Progresses all active animations by the time step and removes those that have finished.
updateAnimation :: Float -> [Animation] -> [Animation]
updateAnimation dt = filter alive . map tick
  where
    tick a =
        let t = animTimer a + dt
         in a{animTimer = t, frameIndex = frameFor a t}
    alive a = animTimer a < animDuration a
    frameFor a t = min (explosionFrames - 1) (floor (t / animDuration a * fromIntegral explosionFrames))

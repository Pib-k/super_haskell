{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Animation where

import Core.Classes
import Core.Constants
import Core.Types
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Entities.Collectible
import Entities.Enemy
import GHC.Generics
import Graphics.Gloss
import System.Random

data AnimationType = DefeatExplosion | MainDoFountain
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Animation = Animation
    { animPos :: Position
    , animType :: AnimationType
    , animTimer :: Float
    , animDuration :: Float
    , frameIndex :: Int
    , seed :: Int
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

explosion :: Enemy -> Animation
explosion e =
    Animation
        { animPos = getPos e
        , animType = DefeatExplosion
        , animTimer = 0
        , animDuration = explosionDuration
        , frameIndex = 0
        , seed = 0
        }

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
        }

instance Renderable Animation where
    render _dbg a =
        uncurry translate (animPos a) $
            case animType a of
                DefeatExplosion -> explosionPicture a
                MainDoFountain -> fountainPicture a

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

fountainPicture :: Animation -> Picture
fountainPicture a = case animType a of
    MainDoFountain ->
        pictures
            [ if tLocal >= 0 && tLocal < 1.2
                then
                    translate (fpVx p * tLocal) (fpVy p * tLocal + 0.5 * gravity * tLocal * tLocal) $
                        color (fpColor p) $
                            scale 0.1 0.1 $
                                text (fpSymbol p)
                else Blank
            | p <- generateParticles $ seed a
            , let tLocal = animTimer a - fpLaunchTime p
            ]
    _ -> Blank

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

updateAnimation :: Float -> [Animation] -> [Animation]
updateAnimation dt = filter alive . map tick
  where
    tick a =
        let t = animTimer a + dt
         in a{animTimer = t, frameIndex = frameFor a t}
    alive a = animTimer a < animDuration a
    frameFor a t = min (explosionFrames - 1) (floor (t / animDuration a * fromIntegral explosionFrames))

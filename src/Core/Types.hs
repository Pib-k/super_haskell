{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Graphics.Gloss (Picture)
import qualified SDL.Mixer as Mixer

type Position = (Float, Float)
type Velocity = (Float, Float)
type BoundingBox = (Float, Float) -- (width, height)

-- | Side of impact from the perspective of the primary collidable entity.
data CollisionDir = FromTop | FromBottom | FromLeft | FromRight
    deriving (Eq, Show)

data Levels = Prelude | MonadMaze | IOUnderground
    deriving (Eq, Show, Enum, FromJSON, ToJSON, Generic, Bounded)

data PauseOption = Resume | ReturnMainMenu
    deriving (Eq, Show, Enum, Bounded, Generic, FromJSON, ToJSON)

{- | Game Over triggers:
  * Normal: Out of lives against enemies.
  * RuntimeException: Falling in Bottom-pit.
  * Victory: Finished the level.
-}
data GameOverReason = Normal | RuntimeException | Victory
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Game Assets
data Assets = Assets
    { playerImg :: Picture
    , typeMismatchImg :: Maybe Picture
    , spaceLeakImg :: Maybe Picture
    , gameMusic :: !Mixer.Music
    }

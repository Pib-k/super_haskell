module Core.Types where

import Graphics.Gloss (Picture)
import qualified SDL.Mixer as Mixer

type Position = (Float, Float)
type Velocity = (Float, Float)
type BoundingBox = (Float, Float) -- (width, height)

data CollisionDir = FromTop | FromBottom | FromLeft | FromRight
    deriving (Eq, Show)

data Assets = Assets
    { playerImg :: Picture
    , typeMismatchImg :: Maybe Picture
    , spaceLeakImg :: Maybe Picture
    , gameMusic :: !Mixer.Music
    }

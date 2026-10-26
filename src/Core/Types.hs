module Core.Types where

type Position = (Float, Float)
type Velocity = (Float, Float)
type BoundingBox = (Float, Float) -- (width, height)

data CollisionDir = FromTop | FromBottom | FromLeft | FromRight
    deriving (Eq, Show)

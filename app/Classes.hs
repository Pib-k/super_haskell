module Classes where

import Graphics.Gloss (Picture)
import Types

data CollisionDir = FromTop | FromBottom | FromLeft | FromRight deriving (Eq, Show)

class Collidable a where
    getPos :: a -> Position
    getBox :: a -> BoundingBox
    checkCollision :: (Collidable a, Collidable b) => a -> b -> Maybe CollisionDir

class Renderable a where
    render :: a -> Picture

instance (Renderable a) => Renderable [a] where
    render = pictures . map render

class Updateable a where
    updateState :: Float -> a -> a

class (Collidable a) => Movable a where
    getVel :: a -> Velocity
    setPos :: Position -> a -> a
    setVel :: Velocity -> a -> a
    applyVelocity :: Float -> a -> a
    applyVelocity dt entity =
        let (x, y) = getPos entity
            (vx, vy) = getVel entity
         in setPos (x + vx * dt, y + vy * dt) entity

class Resetable a where resetToDef :: Position -> a

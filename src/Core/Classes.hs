module Core.Classes where

import Core.Constants (gravity)
import Core.Types
import Graphics.Gloss (Picture, pictures)

class Collidable a where
    getPos :: a -> Position
    getBox :: a -> BoundingBox

-- CollisionDir is from the perspective of a.
-- For example, a hits the ground -> FromBottom
checkCollision :: (Collidable a, Collidable b) => a -> b -> Maybe CollisionDir
checkCollision a b =
    let (aw, ah) = getBox a
        (bw, bh) = getBox b
        (ax, ay) = getPos a
        (bx, by) = getPos b

        overlapX = abs (ax - bx) * 2 < aw + bw
        overlapY = abs (ay - by) * 2 < ah + bh
     in if overlapX && overlapY
            then
                -- Side with largest overlap is the Collision Direction.
                let dx = ax - bx
                    dy = ay - by
                    overlapWidth = (aw + bw) / 2 - abs dx
                    overlapHeight = (ah + bh) / 2 - abs dy
                 in if overlapWidth < overlapHeight
                        then Just (if dx > 0 then FromLeft else FromRight)
                        else Just (if dy > 0 then FromBottom else FromTop)
            else Nothing

class Renderable a where
    render :: Bool -> a -> Picture

instance (Renderable a) => Renderable [a] where
    render dbg = pictures . map (render dbg)

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

    applyGravity :: Float -> a -> a
    applyGravity dt entity = let (vx, vy) = getVel entity in setVel (vx, vy + gravity * dt) entity

class Resetable a where
    resetToDef :: a -> a -> a

class (Movable a) => Grounded a where
    getGrounded :: a -> Bool
    setGrounded :: Bool -> a -> a

snapToTop :: (Grounded a, Collidable b) => a -> b -> a
snapToTop e t =
    let (_, ty) = getPos t
        (_, th) = getBox t
        (_, eh) = getBox e
        (ex, _) = getPos e
        (vx, _) = getVel e
        newY = ty + (th + eh) / 2
     in setPos (ex, newY) (setVel (vx, 0) $ setGrounded True e)

setVx, setVy :: (Movable a) => Float -> a -> a
setVx x e = setVel (x, snd (getVel e)) e
setVy y e = setVel (fst (getVel e), y) e

stepMovement :: (Grounded a) => Float -> a -> a
stepMovement dt = applyVelocity dt . applyGravity dt . setGrounded False

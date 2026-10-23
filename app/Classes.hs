module Classes where

import Graphics.Gloss (Picture, pictures)
import Types

data CollisionDir = FromTop | FromBottom | FromLeft | FromRight deriving (Eq, Show)

class Collidable a where
    getPos :: a -> Position
    getBox :: a -> BoundingBox

instance Collidable Enemy where
    getPos = enemyPos
    getBox = enemyBox

instance Collidable Player where
    getPos = playerPos
    getBox = playerBox

instance Collidable Terrain where
    getPos = terrainPos
    getBox = terrainBox

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

instance Movable Player where
    getVel = playerVel
    setPos pos p = p{playerPos = pos}
    setVel vel p = p{playerVel = vel}

instance Movable Enemy where
    getVel = enemyVel
    setPos pos e = e{enemyPos = pos}
    setVel vel e = e{enemyVel = vel}

class Resetable a where resetToDef :: a -> a -> a

instance Resetable Player where
    resetToDef start cur =
        start
            {
                health = health cur
            ,   score = score cur
            ,   collectedTrues = collectedTrues cur
            }

instance Resetable LevelState where
    resetToDef start cur = 
        cur
            {
                player = resetToDef (player start) (player cur)
            ,   enemies = enemies start
            ,   animations = []
            }



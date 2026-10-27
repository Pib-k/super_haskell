module Entities.Enemy where

import Core.Classes
import Core.Constants
import Core.Types
import Entities.Terrain
import Graphics.Gloss

data TransformerLayer = OuterMaybeT | MiddleStateT | BaseIO
    deriving (Eq, Show)

data EnemyType
    = TypeMismatch {direction :: Float}
    | SpaceLeak {size :: Float}
    | MonadTransformer {layer :: TransformerLayer}
    | StrictnessBang {chargeSpeed :: Float, isCharging :: Bool}
    deriving (Eq, Show)

data Enemy = Enemy
    { enemyPos :: Position
    , enemyBox :: BoundingBox
    , enemyType :: EnemyType
    , enemyVel :: Velocity
    , enemyGrounded :: Bool
    }
    deriving (Eq, Show)

instance Collidable Enemy where
    getPos = enemyPos
    getBox = enemyBox

instance Movable Enemy where
    getVel = enemyVel
    setPos pos e = e{enemyPos = pos}
    setVel vel e = e{enemyVel = vel}

instance Grounded Enemy where
    setGrounded tf p = p{enemyGrounded = tf}
    getGrounded = enemyGrounded

instance Renderable Enemy where
    render dbg e =
        uncurry translate (getPos e) $
            pictures
                [ color orange $ uncurry rectangleSolid (getBox e)
                , color c (uncurry rectangleWire (getBox e))
                ]
      where
        c = if dbg then dim white else makeColor 0.0 0.0 0.0 0.0

stepEnemy :: Float -> [Terrain] -> Enemy -> Enemy
stepEnemy dt ts e = case enemyType e of
    TypeMismatch dir -> walk dt ts dir e
    _ -> e

walk :: Float -> [Terrain] -> Float -> Enemy -> Enemy
walk dt ts dir e
    | blocked || atCliff = landed{enemyType = TypeMismatch (-dir)}
    | otherwise = landed
  where
    moved = stepMovement dt (setVx (dir * walkSpeed) e)
    landed = foldl resolveEnemy moved ts

    isWall c = c == Just FromLeft || c == Just FromRight
    blocked = any (\t -> isSolid (tType t) && isWall (checkCollision landed t)) ts

    (x, y) = getPos landed
    (w, h) = getBox landed
    probe = (x + dir * w / 2, y - h / 2 - 1)
    groundAhead = any (contains probe) ts
    atCliff = enemyGrounded landed && not groundAhead

contains :: Position -> Terrain -> Bool
contains (px, py) t = abs (px - tx) * 2 <= tw && abs (py - ty) * 2 <= th
  where
    (tx, ty) = getPos t
    (tw, th) = getBox t

resolveEnemy :: Enemy -> Terrain -> Enemy
resolveEnemy e t = case checkCollision e t of
    Just FromBottom
        | isSolid (tType t) -> snapToTop e t
    _ -> e

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Entities.Player where

import Core.Classes
import Core.Constants
import Core.Types
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Maybe (isJust)
import Entities.Terrain
import GHC.Generics (Generic)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Key (..), KeyState (..))

data Player = Player
    { playerPos :: Position
    , playerVel :: Velocity
    , playerBox :: BoundingBox
    , isGrounded :: Bool
    , health :: Int
    , score :: Int
    , collectedTrues :: Int
    }
    deriving (Eq, Show, FromJSON, ToJSON, Generic)

instance Collidable Player where
    getPos = playerPos
    getBox = playerBox

instance Movable Player where
    getVel = playerVel
    setPos pos p = p{playerPos = pos}
    setVel vel p = p{playerVel = vel}

instance Resetable Player where
    resetToDef start cur =
        start
            { health = health cur
            , score = score cur
            , collectedTrues = collectedTrues cur
            }

-- | Fallback vector rendering for the Player and debug hitbox outline.
instance Renderable Player where
    render dbg p =
        translate x y $
            pictures
                [ color red $ rectangleSolid bx by
                , color c $ rectangleWire bx by
                ]
      where
        c = if dbg then dim white else makeColor 0.0 0.0 0.0 0.0
        (x, y) = getPos p
        (bx, by) = getBox p

instance Grounded Player where
    setGrounded tf p = p{isGrounded = tf}
    getGrounded = isGrounded

-- | Checks is 'Player' fell too far down.
voidCheck :: Player -> Player
voidCheck p
    | snd (getPos p) < voidY = hurt p
    | otherwise = p

-- | Resets 'Player' position if hurt by an enemy.
hurt :: Player -> Player
hurt p = setVel (0, 0) (setPos spawnPosition p{health = health p - 1})

-- | Upwards boost (used when hitting an enemy from above).
bounce :: Player -> Player
bounce p = setVel (fst (getVel p), bounceSpeed) p

{- | Advances the player by delta time:
- Applies velocity and gravity via 'stepMovement'.
- Resolves collisions against solid terrain and teleporters.
- Applies platform motion from conveyor belts and moving platforms.
-}
stepPlayer :: Float -> [Terrain] -> Player -> Player
stepPlayer dt ts p = voidCheck $ applyGroundMotion dt ts $ foldl resolvePlayer moved ts
  where
    moved = stepMovement dt p

resolvePlayer :: Player -> Terrain -> Player
resolvePlayer p t = case tType t of
    BindTeleporter target | touching -> setPos target p
    tt
        | isSolid tt -> resolve p t
        | otherwise -> p
  where
    touching = isJust (checkCollision p t)

{- | Resolves AABB collisions against solid obstacles:
- FromBottom : Snaps player to platform top.
- FromTop : Bumps ceiling, stopping upward velocity.
- FromLeft/Right: Wall collision, only cancels horizontal velocity if moving into the wall
-}
resolve :: Player -> Terrain -> Player
resolve p t = case checkCollision p t of
    Just FromBottom -> snapToTop p t
    Just FromTop ->
        let newY = ty - (th + ph) / 2
         in setPos (px, newY) (setVel (vx, -1) p)
    Just FromLeft ->
        let newX = tx + (tw + pw) / 2
            velX = max 0 vx
         in setPos (newX, py) (setVel (velX, vy) p)
    Just FromRight ->
        let newX = tx - (tw + pw) / 2
            velX = min 0 vx
         in setPos (newX, py) (setVel (velX, vy) p)
    _ -> p
  where
    (px, py) = getPos p
    (tx, ty) = getPos t
    (tw, th) = getBox t
    (pw, ph) = getBox p
    (vx, vy) = getVel p

-- | Character movement controls
playerKey :: Key -> KeyState -> Player -> Player
playerKey (Char 'a') Down p = setVx (-moveSpeed) p
playerKey (Char 'a') Up p = if fst (getVel p) < 0 then setVx 0 p else p
playerKey (Char 'd') Down p = setVx moveSpeed p
playerKey (Char 'd') Up p = if fst (getVel p) > 0 then setVx 0 p else p
playerKey (Char 'w') Down p
    | isGrounded p = setVy jumpSpeed p
playerKey (Char 's') Down p = setVy fastFallSpeed p
playerKey _ _ p = p

renderPlayerWithSprite :: Picture -> Bool -> Player -> Picture
renderPlayerWithSprite sprite dbg p =
    translate x y $
        pictures
            [scale (0.025 * flipVal) 0.025 sprite, color c $ rectangleWire bx by]
  where
    c = if dbg then dim white else makeColor 0.0 0.0 0.0 0.0
    (x, y) = getPos p
    (vx, _) = getVel p
    (bx, by) = getBox p
    flipVal = if vx < 0.0 then -1.0 else 1.0

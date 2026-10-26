module Entities.Player where

import Core.Classes
import Core.Constants
import Core.Types
import Data.Maybe (isJust)
import Entities.Terrain
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
    deriving (Eq, Show)

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

instance Renderable Player where
    render dbg p = uncurry translate (getPos p) $
        pictures
            [ color red $ uncurry rectangleSolid (getBox p)
            , color c (uncurry rectangleWire (getBox p))
            ]
      where
        c = if dbg then dim white else makeColor 0.0 0.0 0.0 0.0

setVx, setVy :: Float -> Player -> Player
setVx x p = setVel (x, snd (getVel p)) p
setVy y p = setVel (fst (getVel p), y) p

voidCheck :: Player -> Player
voidCheck p
    | snd (getPos p) < voidY = hurt p
    | otherwise = p

hurt :: Player -> Player
hurt p = setVel (0, 0) (setPos spawnPosition p{health = health p - 1})

bounce :: Player -> Player
bounce p = setVel (fst (getVel p), bounceSpeed) p

stepPlayer :: Float -> [Terrain] -> Player -> Player
stepPlayer dt ts p = voidCheck (foldl resolvePlayer moved ts)
  where
    (vx, vy) = getVel p
    falling = setVel (vx, vy + gravity * dt) p
    moved = (applyVelocity dt falling){isGrounded = False}

resolvePlayer :: Player -> Terrain -> Player
resolvePlayer p t = case tType t of
    BottomPit | touching -> hurt p
    BindTeleporter target | touching -> setPos target p
    tt
        | isSolid tt -> resolve p t
        | otherwise -> p
  where
    touching = isJust (checkCollision p t)

resolve :: Player -> Terrain -> Player
resolve p t = case checkCollision p t of
    Just FromBottom ->
        let newY = ty + (th + ph) / 2
         in (setPos (px, newY) (setVel (vx, 0) p)){isGrounded = True}
    Just FromTop ->
        let newY = ty - (th + ph) / 2
         in setPos (px, newY) (setVel (vx, -1) p)
    Just FromLeft ->
        let newX = tx + (tw + pw) / 2
         in setPos (newX, py) (setVel (0, vy) p)
    Just FromRight ->
        let newX = tx - (tw + pw) / 2
         in setPos (newX, py) (setVel (0, vy) p)
    _ -> p
  where
    (px, py) = getPos p
    (tx, ty) = getPos t
    (tw, th) = getBox t
    (pw, ph) = getBox p
    (vx, vy) = getVel p

playerKey :: Key -> KeyState -> Player -> Player
playerKey (Char 'a') Down p = setVx (-moveSpeed) p
playerKey (Char 'a') Up p = if fst (getVel p) < 0 then setVx 0 p else p
playerKey (Char 'd') Down p = setVx moveSpeed p
playerKey (Char 'd') Up p = if fst (getVel p) > 0 then setVx 0 p else p
playerKey (Char 'w') Down p
    | isGrounded p = setVy jumpSpeed p
playerKey (Char 's') Down p = setVy fastFallSpeed p
playerKey _ _ p = p

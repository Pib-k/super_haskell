{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Entities.Enemy where

import Core.Classes
import Core.Constants
import Core.Types
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Maybe (isJust)
import Entities.Terrain
import GHC.Generics (Generic)
import Graphics.FontMetrics
import Graphics.Gloss

{- | Layers of the MonadTransformer enemy. Each stomp unwraps a layer:
  OuterMaybeT (slowest, largest) -> MiddleStateT -> BaseIO (fastest, smallest, jumping).
-}
data TransformerLayer = OuterMaybeT | MiddleStateT | BaseIO
    deriving (Eq, Show, FromJSON, ToJSON, Generic)

layerSpeed :: TransformerLayer -> Float
layerSpeed OuterMaybeT = 60.0
layerSpeed MiddleStateT = 120.0
layerSpeed BaseIO = 180.0

layerBox :: TransformerLayer -> BoundingBox
layerBox OuterMaybeT = (30.0, 30.0)
layerBox MiddleStateT = (22.0, 22.0)
layerBox BaseIO = (14.0, 14.0)

chargeSpeed :: Float
chargeSpeed = 300.0

{- | Enemies themed around Haskell runtime and type system concepts:
  * TypeMismatch : Rigid block patrolling platforms. Reverses direction at walls and cliffs.
  * SpaceLeak : Unevaluated thunk accumulating memory, expanding its bounding box as time goes on.
  * MonadTransformer : Multi-layered enemy needing multiple stomps to kill.
  * StrictnessBang : Proximity enemy that forces evaluation by charging at the player.
-}
data EnemyType
    = TypeMismatch {direction :: Float}
    | SpaceLeak {size :: Float}
    | MonadTransformer {layer :: TransformerLayer}
    | StrictnessBang {bangPhase :: BangPhase}
    deriving (Eq, Show, FromJSON, ToJSON, Generic)

{- | State machine for StrictnessBang (!):
IdleThunk -> Forcing -> StrictCharge -> NormalForm.
-}
data BangPhase
    = IdleThunk
    | Forcing Float
    | StrictCharge Float Float
    | NormalForm Float
    deriving (Eq, Show, FromJSON, ToJSON, Generic)

data Enemy = Enemy
    { enemyPos :: Position
    , enemyBox :: BoundingBox
    , enemyType :: EnemyType
    , enemyVel :: Velocity
    , enemyGrounded :: Bool
    }
    deriving (Eq, Show, FromJSON, ToJSON, Generic)

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

-- Rendering of 'Enemy', see fontmetrics module for text creation
instance Renderable Enemy where
    render dbg e = case enemyType e of
        SpaceLeak _ -> translate x y $ pictures [color green $ rectangleWire w h, color c $ rectangleWire w h]
        StrictnessBang _ ->
            let m = charMetrics '!'
                (s, dy) = textLayout h m
             in color green $
                    translate x y $
                        rotate (-90) $
                            pictures
                                [drawTextAt s (-w / 2) dy "!", color c $ rectangleWire w h]
        MonadTransformer l ->
            let col = case l of
                    OuterMaybeT -> color blue
                    MiddleStateT -> color green
                    BaseIO -> color white
             in translate x y $ pictures [col $ rectangleWire w h, color c $ rectangleWire w h]
        TypeMismatch _ ->
            translate x y $
                pictures
                    [ color red $ rectangleSolid w h
                    , color c $ rectangleWire w h
                    ]
      where
        c = if dbg then dim white else makeColor 0.0 0.0 0.0 0.0
        (w, h) = getBox e
        (x, y) = getPos e

renderEnemyWithSprite :: Maybe Picture -> Bool -> Enemy -> Picture
renderEnemyWithSprite maybeSprite dbg e = case maybeSprite of
    Nothing -> render dbg e
    Just sprite ->
        translate x y $
            pictures
                [ scale (0.1 * flipVal) 0.1 sprite
                , color c $ rectangleWire bx by
                ]
  where
    c = if dbg then dim white else makeColor 0.0 0.0 0.0 0.0
    (x, y) = getPos e
    (vx, _) = getVel e
    (bx, by) = getBox e
    flipVal = if vx < 0.0 then -1.0 else 1.0

stepEnemy :: Float -> Position -> [Terrain] -> Enemy -> Enemy
stepEnemy dt playerPos ts e = case enemyType e of
    TypeMismatch dir -> walk dt ts dir e
    SpaceLeak s -> e{enemyBox = (w + s * dt, h + s * dt)}
    StrictnessBang phase -> stepBang dt playerPos ts e phase
    MonadTransformer l -> stepTransformer dt playerPos ts l e
  where
    (w, h) = getBox e

-- | StrictnessBang scans in x-axis for player and if player within range, attack the player.
stepBang :: Float -> Position -> [Terrain] -> Enemy -> BangPhase -> Enemy
stepBang dt (px, py) ts bang phase = case phase of
    IdleThunk ->
        if inSight
            then setPhase (Forcing 0.4) bang
            else bang
    Forcing t
        | t - dt <= 0 ->
            let dir = if px >= ex then 1.0 else -1.0
                chargeDuration = 1.0
             in setPhase (StrictCharge dir chargeDuration) bang
        | otherwise ->
            setPhase (Forcing (t - dt)) bang
    StrictCharge dir timeLeft
        | timeLeft - dt <= 0 || hitWall ->
            setPhase (NormalForm 1.5) bang
        | otherwise ->
            let newX = ex + dir * chargeSpeed * dt
             in (setPhase (StrictCharge dir (timeLeft - dt)) bang){enemyPos = (newX, ey)}
    NormalForm t
        | t - dt <= 0 ->
            setPhase IdleThunk bang
        | otherwise ->
            setPhase (NormalForm (t - dt)) bang
  where
    (ex, ey) = getPos bang
    inSight = abs (py - ey) < 30.0 && abs (px - ex) < 250.0
    hitWall = any (\t -> isSolid (tType t) && isJust (checkCollision bang t)) ts
    setPhase p e = e{enemyType = StrictnessBang p}

-- | Platform patrol: checks for walls and probes ahead for cliffs to turn around safely.
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

{- | Player stomps an enemy from above:
Returns 'Just newEnemy' when unwrapping a MonadTransformer layer, or 'Nothing' on final defeat.
-}
stompEnemy :: Enemy -> Maybe Enemy
stompEnemy e = case enemyType e of
    MonadTransformer OuterMaybeT ->
        Just
            e
                { enemyType = MonadTransformer MiddleStateT
                , enemyBox = (22, 22)
                }
    MonadTransformer MiddleStateT ->
        Just
            e
                { enemyType = MonadTransformer BaseIO
                , enemyBox = (14, 14)
                }
    MonadTransformer BaseIO ->
        Nothing
    _ -> Nothing

-- | Similar to walk function, with added functionality of moving to player position.
stepTransformer :: Float -> Position -> [Terrain] -> TransformerLayer -> Enemy -> Enemy
stepTransformer dt (px, _) ts l e
    | blocked || atCliff = setVx (-dir * spd) landed
    | otherwise = landed'
  where
    spd = layerSpeed l
    (ex, _) = getPos e
    dir = if px >= ex then 1.0 else -1.0

    moved = stepMovement dt (setVx (dir * spd) e)
    landed = foldl resolveEnemy moved ts

    isWall c = c == Just FromLeft || c == Just FromRight
    blocked = any (\t -> isSolid (tType t) && isWall (checkCollision landed t)) ts
    (x, y) = getPos landed
    (w, h) = getBox landed
    probe = (x + dir * w / 2, y - h / 2 - 1)
    groundAhead = any (contains probe) ts
    atCliff = enemyGrounded landed && not groundAhead

    landed'
        | l == BaseIO && enemyGrounded landed = setVy 250.0 landed
        | otherwise = landed

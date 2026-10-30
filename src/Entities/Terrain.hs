{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Entities.Terrain where

import Core.Classes
import Core.Constants
import Core.Types
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import GHC.Generics
import Graphics.FontMetrics
import Graphics.Gloss

data TerrainType
    = SolidList
    | FragileComment {opacity :: Float, isTriggered :: Bool}
    | BottomPit
    | BindTeleporter {targetPos :: Position}
    | ConveyorBelt {beltSpeed :: Float}
    | PatternGuard {isLocked :: Bool, requiredTrues :: Int}
    | InfiniteList {currentOffset :: Float, maxOffset :: Float, velocity :: Float}
    | MainDoGoal
    | UnitPlatform
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Terrain = Terrain
    { terrainPos :: Position
    , terrainBox :: BoundingBox
    , tType :: TerrainType
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Collidable Terrain where
    getPos = terrainPos
    getBox = terrainBox

surfaceVelocity :: TerrainType -> Float
surfaceVelocity (ConveyorBelt spd) = spd
surfaceVelocity (InfiniteList _ _ vel) = vel
surfaceVelocity _ = 0.0

applyGroundMotion :: (Collidable a, Movable a) => Float -> [Terrain] -> a -> a
applyGroundMotion dt ts entity = case standingOn entity ts of
    Just t ->
        let vx = surfaceVelocity (tType t)
            (x, y) = getPos entity
         in if vx == 0
                then entity
                else setPos (x + vx * dt, y) entity
    Nothing -> entity

isSolid :: TerrainType -> Bool
isSolid SolidList = True
isSolid (FragileComment opac _) = opac > 0.0
isSolid ConveyorBelt{} = True
isSolid UnitPlatform = True
isSolid InfiniteList{} = True
isSolid (PatternGuard locked _) = locked
isSolid _ = False

{-- | Given a TerrainType and a width and height, returns a constrained
-- BoundingBox that matches the rendered font metrics.
--}
constrainTerrainBox :: TerrainType -> BoundingBox -> BoundingBox
constrainTerrainBox tt (targetW, targetH) =
    case tt of
        SolidList ->
            let minW = segWidth '[' "[]"
             in (max minW targetW, h)
        BottomPit ->
            (snapMultiple (segWidth '|' "_|_"), h)
        FragileComment _ _ ->
            (snapMultiple (segWidth '-' "--"), h)
        ConveyorBelt spd ->
            let sym = if spd >= 0 then "->" else "<-"
             in (snapMultiple (segWidth '>' sym), h)
        BindTeleporter _ ->
            (segWidth '>' ">>=", h)
        PatternGuard _ _ ->
            (segWidth '|' "|", h)
        UnitPlatform ->
            (segWidth '(' "()", h)
        MainDoGoal ->
            (segWidth 'd' "main = do", h)
        InfiniteList _ _ _ ->
            (segWidth '[' "[..]", h)
  where
    h = max 1.0 targetH
    snapMultiple :: Float -> Float
    snapMultiple segW =
        let k = max 1 (round (targetW / segW) :: Int)
         in fromIntegral k * segW
    segWidth :: Char -> String -> Float
    segWidth refChar sym =
        let m = charMetrics refChar
            ch = if charHeight m <= 0 then 100.0 else charHeight m
         in h * (stringAdvance sym / ch)

mkTerrain :: Position -> BoundingBox -> TerrainType -> Terrain
mkTerrain pos rawBox tt = Terrain pos (constrainTerrainBox tt rawBox) tt

stepTerrain :: Float -> Terrain -> Terrain
stepTerrain ds t@(Terrain _ _ (FragileComment opac isTrigg))
    | isTrigg = t{tType = FragileComment (max 0.0 (opac - ds / 0.8)) isTrigg}
    | otherwise = t
stepTerrain ds (Terrain (x, y) box (InfiniteList cOff mOff vel)) =
    Terrain (x + vel * ds, y) box (InfiniteList finalOffset mOff newVel)
  where
    nextOffset = cOff + vel * ds
    (newVel, finalOffset)
        | nextOffset >= mOff = (-vel, mOff)
        | nextOffset <= 0.0 = (-vel, 0.0)
        | otherwise = (vel, nextOffset)
stepTerrain _ tt = tt

instance Renderable Terrain where
    render dbg (Terrain tPos tBox terrainType) =
        uncurry translate tPos $
            pictures
                [ drawTerrainType terrainType tBox
                , color c (uncurry rectangleWire tBox)
                ]
      where
        c = if dbg then dim white else makeColor 0.0 0.0 0.0 0.0

drawTerrainType :: TerrainType -> BoundingBox -> Picture
drawTerrainType tt (w, h) = case tt of
    SolidList ->
        color white $
            let m = charMetrics '['
                (s, dy) = textLayout h m
                bracketW = charAdvance m * s
             in pictures
                    [ drawTextAt s (-w / 2) dy "["
                    , drawTextAt s (w / 2 - bracketW) dy "]"
                    ]
    BottomPit ->
        let sym = "_|_"
            m = charMetrics '|'
            (s, dy) = textLayout h m
            segW = stringAdvance sym * s
            count = max 1 (round (w / segW))
         in color red $
                pictures
                    [ drawTextAt s x dy sym
                    | i <- [0 .. count - 1]
                    , let x = -w / 2 + fromIntegral i * segW
                    ]
    FragileComment opac _
        | opac <= 0.0 -> Blank
        | otherwise ->
            let sym = "--"
                m = charMetrics '-'
                (s, dy) = textLayout h m
                segW = stringAdvance sym * s
                count = max 1 (round (w / segW))
             in color (makeColor opac opac opac opac) $
                    pictures
                        [ drawTextAt s x dy sym
                        | i <- [0 .. count - 1]
                        , let x = -w / 2 + fromIntegral i * segW
                        ]
    BindTeleporter _ ->
        let sym = ">>="
            m = charMetrics '>'
            (s, dy) = textLayout h m
            segW = stringAdvance sym * s
         in color green $ drawTextAt s (-segW / 2) dy sym
    ConveyorBelt spd ->
        let sym = if spd >= 0 then "->" else "<-"
            m = charMetrics '>'
            (s, dy) = textLayout h m
            segW = stringAdvance sym * s
            count = max 1 (round (w / segW))
         in color yellow $
                pictures
                    [ drawTextAt s x dy sym
                    | i <- [0 .. count - 1]
                    , let x = -w / 2 + fromIntegral i * segW
                    ]
    PatternGuard locked _
        | locked ->
            let m = charMetrics '|'
                (s, dy) = textLayout h m
                segW = charAdvance m * s
             in color white $ drawTextAt s (-segW / 2) dy "|"
        | otherwise -> Blank
    UnitPlatform ->
        let sym = "()"
            m = charMetrics '('
            (s, dy) = textLayout h m
            textW = stringAdvance sym * s
         in color cyan $ drawTextAt s (-textW / 2) dy sym
    MainDoGoal ->
        let sym = "main=do"
            m = charMetrics 'd'
            (s, dy) = textLayout h m
            textW = stringAdvance sym * s
         in color yellow $ drawTextAt s (-textW / 2) dy sym
    InfiniteList _ _ _ ->
        let sym = "[..]"
            m = charMetrics '['
            (s, dy) = textLayout h m
            textW = stringAdvance sym * s
         in color magenta $ drawTextAt s (-textW / 2) dy sym

drawTextAt :: Float -> Float -> Float -> String -> Picture
drawTextAt s x y str =
    translate x y $ scale s s (text str)

textLayout :: Float -> CharMetrics -> (Float, Float)
textLayout boxHeight m =
    let s = boxHeight / max 1.0 (charHeight m)
        dy = -(charCenterY m * s) - padding
     in (s, dy)

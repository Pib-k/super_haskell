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

{- | Environmental features rendered as Haskell syntax glyphs:
  * SolidList : Bracket platforms ([ ]) supporting player and enemies.
  * FragileComment : Decaying comment lines (--) fading away shortly after contact.
  * BottomPit : Pit hazard (_|_) representing undefined / bottom.
  * BindTeleporter : Monadic bind (>>=) instantly transporting the player to target coordinates.
  * ConveyorBelt : Function arrows (-> / <-) pushing entities along the surface.
  * PatternGuard : Guard barrier (|) blocking passage until required True count is met.
  * InfiniteList : Moving platform ([..]) lazily cycling between predefined offsets.
  * MainDoGoal : Exit flagpole (main = do) triggering level completion.
  * UnitPlatform : Small stepping stones (()).
-}
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

-- | Adds horizontal velocity (conveyor belt or moving platform) to entities standing on top.
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
            (flagThickness, flagHeight)
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

-- | Returns a valid bounding box for the given terrain, as it needs to align with the fontmetrics.
mkTerrain :: Position -> BoundingBox -> TerrainType -> Terrain
mkTerrain pos rawBox tt = Terrain pos (constrainTerrainBox tt rawBox) tt

{- | Pure delta-time update for dynamic terrain:
  * FragileComment : Fades opacity over 0.8 seconds once stepped on.
  * InfiniteList : Moves horizontally and bounces at offset limits.
-}
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

-- | Drawing of Terrain based on given fontmetrics with debug bounding box rendering if enabled.
instance Renderable Terrain where
    render dbg (Terrain tPos tBox terrainType) =
        uncurry translate tPos $
            pictures
                [ drawTerrainType terrainType tBox
                , color c (uncurry rectangleWire tBox)
                ]
      where
        c = if dbg then dim white else makeColor 0.0 0.0 0.0 0.0

-- Look at fontmetrics for the special values needed to render this text properly.
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
            count = max 1 (round (w / segW)) :: Integer
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
                count = max 1 (round (w / segW)) :: Integer
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
            count = max 1 (round (w / segW)) :: Integer
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
        let sym = "main = do"
            m = charMetrics 'd'
            s = h / stringAdvance sym
            dy = -(charCenterY m * s)
            textW = stringAdvance sym * s
            poleX = -w / 2 + 2
            flagW = w * 0.9
            flagH = h * 0.22
            top = h / 2
         in pictures
                [ color orange $ translate poleX 0 (rectangleSolid 3 h)
                , color yellow $ translate 2 0 $ rotate (-90) $ drawTextAt s (-textW / 2) dy sym
                , color red $
                    polygon
                        [ (poleX, top)
                        , (poleX - flagW, top - flagH / 2)
                        , (poleX, top - flagH)
                        ]
                , color (dark red) $ translate poleX top (circleSolid 3)
                ]
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

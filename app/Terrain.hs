module Terrain where

import FontMetrics
import Types

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
        InfiniteList _ ->
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

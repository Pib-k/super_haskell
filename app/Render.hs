module Render where

import Classes
import FontMetrics
import Graphics.Gloss
import Types

instance Renderable Player where
    render p = uncurry translate (getPos p) $ color red $ uncurry rectangleSolid (getBox p)

instance Renderable Terrain where
    render (Terrain tPos tBox tType) = uncurry translate tPos $ color white $ (drawTerrainType tType tBox)

drawTerrainType :: TerrainType -> BoundingBox -> Picture
drawTerrainType tt (w, h) = case tt of
    SolidList ->
        let m = charMetrics '['
            s = h / charHeight m
            dy = -(charCenterY m * s) - padding
            bracketW = charAdvance m * s
         in pictures
                [ translate (-w / 2) dy $ scale s s (text "[")
                , translate (w / 2 - bracketW) dy $ scale s s (text "]")
                ]
    BottomPit ->
        let sym = "_|_"
            m = charMetrics '|'
            s = h / charHeight m
            dy = -(charCenterY m * s) - padding
            segW = stringAdvance sym * s
            count = max 1 (floor (w / segW))
         in color red $
                pictures
                    [ translate x dy $ scale s s (text sym)
                    | i <- [0 .. count - 1]
                    , let x = -w / 2 + fromIntegral i * segW
                    ]
    _ -> color red $ rectangleSolid w h
  where
    padding = 1.0

viewPure :: GameState -> Picture
viewPure gs = case appMode gs of
    MainMenu -> label "Super Haskell - Press Enter"
    LevelSelection -> label (availableLevels gs !! levelIndex gs ++ " (A/D, Enter)")
    Playing ->
        let lvl = currentLevel gs
         in pictures [render (terrain lvl), render (player lvl)]
  where
    label = translate (-300) 0 . scale 0.2 0.2 . color white . text

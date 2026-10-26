module Render where

import Classes
import FontMetrics
import Graphics.Gloss
import Types

instance Renderable Player where
    render dbg p = uncurry translate (getPos p) $ color red $ uncurry rectangleSolid (getBox p)
      where
        c = if dbg then (dim white) else makeColor 0.0 0.0 0.0 0.0

instance Renderable Terrain where
    -- BOUNDING BOX RENDERING FOR DEBUGGING
    render dbg (Terrain tPos tBox tType) =
        uncurry translate tPos $
            pictures
                [ drawTerrainType tType tBox
                , color c (uncurry rectangleWire tBox)
                ]
      where
        c = if dbg then (dim white) else makeColor 0.0 0.0 0.0 0.0

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
    FragileComment opac _ ->
        let sym = "--"
            m = charMetrics '-'
            (s, dy) = textLayout h m
            segW = stringAdvance sym * s
            count = max 1 (round (w / segW))
         in color (makeColor 1.0 1.0 1.0 opac) $
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
    PatternGuard _ _ ->
        let m = charMetrics '|'
            (s, dy) = textLayout h m
            segW = charAdvance m * s
         in color white $ drawTextAt s (-segW / 2) dy "|"
    UnitPlatform ->
        let sym = "()"
            m = charMetrics '('
            (s, dy) = textLayout h m
            textW = stringAdvance sym * s
         in color cyan $ drawTextAt s (-textW / 2) dy sym
    MainDoGoal ->
        let sym = "main = do"
            m = charMetrics 'd'
            (s, dy) = textLayout h m
            textW = stringAdvance sym * s
         in color yellow $ drawTextAt s (-textW / 2) dy sym
    InfiniteList offset ->
        let sym = "[..]"
            m = charMetrics '['
            (s, dy) = textLayout h m
            textW = stringAdvance sym * s
         in color magenta $ drawTextAt s (-textW / 2 + offset) dy sym
    _ -> color red $ rectangleSolid w h
  where
    padding = 1.0

    textLayout boxHeight m =
        let s = boxHeight / max 1.0 (charHeight m)
            dy = -(charCenterY m * s) - padding
         in (s, dy)

    drawTextAt s x y str =
        translate x y $ scale s s (text str)

instance Renderable Enemy where
    render e = uncurry translate (getPos e) $ color orange $ uncurry rectangleSolid (getBox e)

viewPure :: GameState -> Picture
viewPure gs = case appMode gs of
    MainMenu -> label "Super Haskell - Press Enter"
    LevelSelection -> label (availableLevels gs !! levelIndex gs ++ " (A/D, Enter)")
    Playing ->
        let lvl = currentLevel gs
            dbg = isDebugMode gs
         in pictures [render dbg (terrain lvl), render dbg (player lvl), render (enemies lvl)]
  where
    label = translate (-300) 0 . scale 0.2 0.2 . color white . text

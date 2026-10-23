module Render where

import Classes
import Graphics.Gloss
import Types

instance Renderable Player where
    render p = uncurry translate (getPos p) $ color red $ uncurry rectangleSolid (getBox p)

instance Renderable Terrain where
    render t = uncurry translate (getPos t) $ color green $ uncurry rectangleSolid (getBox t)

instance Renderable Enemy where
    render e = uncurry translate (getPos e) $ color orange $ uncurry rectangleSolid (getBox e)

viewPure :: GameState -> Picture
viewPure gs = case appMode gs of
    MainMenu -> label "Super Haskell - Press Enter"
    LevelSelection -> label (availableLevels gs !! levelIndex gs ++ " (A/D, Enter)")
    Playing -> 
        let lvl = currentLevel gs
        in pictures [render (terrain lvl), render (player lvl), render (enemies lvl)]
    where
        label = translate (-300) 0 . scale 0.2 0.2 . color white . text



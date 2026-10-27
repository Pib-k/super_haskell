module Graphics.Render where

import Core.Classes
import Entities.Enemy ()
import Entities.Player ()
import Entities.Terrain ()
import Game.State
import Graphics.Gloss

viewPure :: GameState -> Picture
viewPure gs = case appMode gs of
    MainMenu -> label "Super Haskell - Press Enter"
    LevelSelection -> label (availableLevels gs !! levelIndex gs ++ " (A/D, Enter)")
    Playing ->
        let lvl = currentLevel gs
            dbg = isDebugMode gs
         in pictures [render dbg (terrain lvl), render dbg (player lvl), render dbg (enemies lvl), render dbg (collectibles lvl)]
  where
    label = translate (-300) 0 . scale 0.2 0.2 . color white . text

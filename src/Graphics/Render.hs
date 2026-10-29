module Graphics.Render where

import Core.Classes
import Core.Types
import Entities.Enemy
import Entities.Player (renderPlayerWithSprite)
import Entities.Terrain ()
import Game.State
import Graphics.Gloss

viewPure :: Assets -> GameState -> Picture
viewPure assets gs =
    case appMode gs of
        MainMenu -> label "Super Haskell - Press Enter"
        LevelSelection ->
            label (availableLevels gs !! levelIndex gs ++ " (A/D, Enter)")
        Playing ->
            let lvl = currentLevel gs
                dbg = isDebugMode gs
             in pictures
                    [ render dbg (terrain lvl)
                    , renderPlayerWithSprite (playerImg assets) dbg (player lvl)
                    , pictures (map (\e -> renderEnemyWithSprite (spriteFor e) dbg e) (enemies lvl))
                    , render dbg (collectibles lvl)
                    , render dbg (animations lvl)
                    ]
  where
    label = translate (-300) 0 . scale 0.2 0.2 . color white . text

spriteFor :: Enemy -> Maybe Picture
spriteFor e = case enemyType e of
    _ -> Nothing

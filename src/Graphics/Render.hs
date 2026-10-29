module Graphics.Render where

import Core.Classes
import Core.Constants
import Entities.Enemy (renderEnemyWithSprite)
import Entities.Player (renderPlayerWithSprite)
import Entities.Terrain ()
import Entities.Collectible (CollectibleType (..), drawCollectibleType)
import Game.State
import Graphics.Gloss
import Graphics.RenderAnim

viewPure :: Picture -> Picture -> GameState -> Picture
viewPure playerSprite enemySprite gs =
    case appMode gs of
        MainMenu -> label "Super Haskell - Press Enter"
        LevelSelection ->
            label (availableLevels gs !! levelIndex gs ++ " (A/D, Enter)")
        Playing ->
            let lvl = currentLevel gs
                dbg = isDebugMode gs
             in pictures
                    [ render dbg (terrain lvl)
                    , renderPlayerWithSprite playerSprite dbg (player lvl)
                    , pictures (map (renderEnemyWithSprite enemySprite dbg) (enemies lvl))
                    , render dbg (collectibles lvl)
                    , render dbg (animations lvl)
                    ]
  where
    label = translate (-300) 0 . scale 0.2 0.2 . color white . text
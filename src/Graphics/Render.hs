module Graphics.Render where

import Core.Classes
import Core.Types
import Entities.Enemy
import Entities.Player (renderPlayerWithSprite, Player (..))
import Entities.Terrain ()
import Graphics.Camera (withCamera)
import Graphics.Menu
import Game.State
import Graphics.Gloss

viewPure :: Assets -> GameState -> Picture
viewPure assets gs =
    case appMode gs of
        MainMenu -> mainMenuPicture gs
        Controls -> controlsPicture
        LevelSelection -> levelSelectPicture gs
        Playing ->
            let lvl = currentLevel gs
                dbg = isDebugMode gs
             in withCamera lvl $ pictures
                    [ render dbg (terrain lvl)
                    , renderPlayerWithSprite (playerImg assets) dbg (player lvl)
                    , pictures (map (\e -> renderEnemyWithSprite (spriteFor e) dbg e) (enemies lvl))
                    , render dbg (collectibles lvl)
                    , render dbg (animations lvl)
                    ]
        LevelComplete ->
            let p = player (currentLevel gs)
            in pictures
                    [ textAt 100 0.4 "Level Complete!"
                    , textAt 20 0.2 ("Score: " ++ show (score p))
                    , textAt (-30) 0.2 ("Lives: " ++ show (health p))
                    , textAt (-100) 0.15 "Press Enter to continue"
                    ]
        GameOver ->
            let p = player (currentLevel gs)
            in pictures
                    [ translate (-300) 100 $ scale 0.5 0.5 $ color red $ text "Game Over"
                    , textAt 20 0.2 ("Score: " ++ show (score p))
                    , textAt (-30) 0.2 ("High score: " ++ show (highScore gs))
                    , textAt (-100) 0.15 "Enter: retry    M: main menu"
                    ]
  where
    textAt y s = translate (-300) y . scale s s . color white . text

spriteFor :: Enemy -> Maybe Picture
spriteFor e = case enemyType e of
    _ -> Nothing


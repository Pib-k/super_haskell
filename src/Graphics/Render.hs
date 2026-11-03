module Graphics.Render where

import Core.Classes
import Core.Types
import Entities.Enemy
import Entities.Player (Player (..), renderPlayerWithSprite)
import Entities.Terrain ()
import Game.State
import Graphics.Camera (withCamera)
import Graphics.Gloss
import Graphics.Lives
import Graphics.Menu

viewPure :: Assets -> GameState -> Picture
viewPure assets gs =
    case appMode gs of
        MainMenu -> mainMenuPicture gs
        Controls -> controlsPicture
        LevelSelection -> levelSelectPicture gs
        Playing -> renderWorld
        LevelComplete ->
            let p = player (levelState gs)
             in pictures
                    [ textAt 100 0.4 "Level Complete!"
                    , textAt 20 0.2 ("Score: " ++ show (score p))
                    , textAt (-30) 0.2 ("Lives: " ++ show (health p))
                    , textAt (-100) 0.15 "Press Enter to continue"
                    ]
        GameOver ->
            let p = player (levelState gs)
             in pictures
                    [ translate (-300) 100 $ scale 0.5 0.5 $ color red $ text "Game Over"
                    , textAt 20 0.2 ("Score: " ++ show (score p))
                    , textAt (-30) 0.2 ("High score: " ++ show (highScore gs))
                    , textAt (-100) 0.15 "Enter: retry    M: main menu"
                    ]
        Paused opt -> pictures [renderWorld, pauseMenuPicture opt]
  where
    renderWorld =
        let lvl = levelState gs
            dbg = isDebugMode gs
         in pictures
                [ withCamera lvl $
                    pictures
                        [ render dbg (terrain lvl)
                        , renderPlayerWithSprite (playerImg assets) dbg (player lvl)
                        , pictures (map (\e -> renderEnemyWithSprite (spriteFor e) dbg e) (enemies lvl))
                        , render dbg (collectibles lvl)
                        , render dbg (animations lvl)
                        ]
                , livesHud (health (player lvl))
                ]
    textAt y s = translate (-300) y . scale s s . color white . text

spriteFor :: Enemy -> Maybe Picture
spriteFor e = case enemyType e of
    _ -> Nothing

module Graphics.Render where

import Core.Classes
import Core.Types
import Entities.Enemy
import Entities.Player (Player (..), renderPlayerWithSprite)
import Entities.Terrain ()
import Game.State
import Graphics.Camera (withCamera)
import Graphics.Gloss
import Graphics.Hud
import Graphics.Menu

viewPure :: Assets -> GameState -> Picture
viewPure assets gs =
    case appMode gs of
        MainMenu -> mainMenuPicture gs
        Controls -> controlsPicture
        LevelSelection -> levelSelectPicture gs
        Playing -> renderWorld
        LevelComplete ->
            pictures
                [ centered 0.45 90 yellow "Level Complete!"
                , centered 0.2 25 white ("Score: " ++ show (score p))
                , centered 0.2 (-25) (greyN 0.8) ("Lives: " ++ show (health p))
                , centered 0.14 (-100) (greyN 0.6) "Press Enter to continue"
                ]
        GameOver reason -> case reason of
            RuntimeException ->
                pictures
                    [ centered 0.30 90 red "*** Exception: Prelude.undefined"
                    , centered 0.18 30 (greyN 0.8) "Fell into _|_ (Bottom)"
                    , centered 0.18 (-20) white ("Score: " ++ show (score p))
                    , centered 0.14 (-100) (greyN 0.6) "Enter: retry    M: main menu"
                    ]
            Normal ->
                pictures
                    [ centered 0.5 90 red "Game Over"
                    , centered 0.2 25 white ("Score: " ++ show (score p))
                    , centered 0.2 (-25) (greyN 0.8) ("High score: " ++ show (highScore gs))
                    , centered 0.14 (-100) (greyN 0.6) "Enter: retry    M: main menu"
                    ]
            Victory ->
                pictures
                    [ centered 0.45 90 yellow "ExitCode: 0 (Victory!)"
                    , centered 0.18 30 (greyN 0.8) "All Monad transformations and IO completed!"
                    , centered 0.2 (-20) white ("Final Score: " ++ show (score p + globalScore gs))
                    , centered 0.14 (-100) (greyN 0.6) "Enter: play again    M: main menu"
                    ]
        Paused opt -> pictures [renderWorld, pauseMenuPicture opt]
  where
    p = player (levelState gs)
    renderWorld =
        let lvl = levelState gs
            dbg = isDebugMode gs
         in pictures
                [ withCamera (fst (windowDim gs) / 2) lvl $
                    pictures
                        [ render dbg (terrain lvl)
                        , renderPlayerWithSprite (playerImg assets) dbg (player lvl)
                        , pictures (map (\e -> renderEnemyWithSprite (spriteFor e) dbg e) (enemies lvl))
                        , render dbg (collectibles lvl)
                        , render dbg (animations lvl)
                        ]
                , gameHud gs
                ]

spriteFor :: Enemy -> Maybe Picture
spriteFor e = case enemyType e of
    _ -> Nothing

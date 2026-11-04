module Graphics.Lives where

import Core.Constants
import Entities.Player (Player (..))
import Game.State (GameState (..), LevelState (..))
import Graphics.FontMetrics (stringAdvance)
import Graphics.Gloss
import Graphics.Menu (levelTitle)

gameHud :: GameState -> Picture
gameHud gs =
    pictures
        [ livesRow
        , levelRow
        , scoreRow
        , truesRow
        ]
  where
    p = player (levelState gs)
    hp = health p
    s = 0.14
    (winW, winH) = windowDim gs
    halfW = winW / 2
    halfH = winH / 2

    leftX = -halfW + 30
    rightX = halfW - 30
    y1 = halfH - 35
    y2 = halfH - 58
    shadowText x y col str =
        pictures
            [ translate (x + 1.2) (y - 1.2) $ scale s s $ color (makeColor 0 0 0 0.85) $ text str
            , translate x y $ scale s s $ color col $ text str
            ]

    rightShadowText rx y col str =
        let w = stringAdvance str * s
         in shadowText (rx - w) y col str

    labelStr = "Lives: "
    startX = leftX + stringAdvance labelStr * s
    iconSpacing = stringAdvance "(<>) " * s

    livesRow =
        pictures $
            shadowText leftX y1 white labelStr
                : [ let col = if i < hp then makeColor 0.2 0.9 0.3 1 else makeColor 0.3 0.3 0.3 0.4
                     in shadowText (startX + fromIntegral i * iconSpacing) y1 col "(<>)"
                  | i <- [0 .. maxHealth - 1]
                  ]
    levelRow =
        shadowText leftX y2 (greyN 0.65) (levelTitle (level gs))

    scoreRow =
        rightShadowText rightX y1 yellow ("Score: " ++ show (score p))

    truesRow =
        rightShadowText rightX y2 cyan ("True: " ++ show (collectedTrues p))

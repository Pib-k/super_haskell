module Graphics.Menu where

import Core.Classes
import Core.Types
import Entities.Enemy
import Entities.Player (renderPlayerWithSprite, Player (..))
import Entities.Terrain ()
import Graphics.Camera (withCamera)
import Graphics.FontMetrics
import Core.Constants
import Game.State
import Graphics.Gloss


centered :: Float -> Float -> Color -> String -> Picture
centered s y c str =
    translate (negate (stringAdvance str * s) / 2) y $ scale s s $ color c $ text str

mainMenuPicture :: GameState -> Picture
mainMenuPicture gs =
    pictures
        [ backdrop
        , centered 0.5 130 yellow "Super Haskell"
        , centered 0.15 95 (greyN 0.6) "main :: IO ()"
        , pictures (zipWith item [0 ..] menuItems)
        , centered 0.12 (-230) (greyN 0.5) "W/S: select    Enter: confirm"
        , centered 0.12 (-255) (greyN 0.5) ("High score: " ++ show (highScore gs))
        ]
  where
    item :: Int -> String -> Picture
    item i label
        | i == menuIndex gs = centered 0.25 y yellow ("> " ++ label ++ " <")
        | otherwise = centered 0.2 y (greyN 0.7) label
      where
        y = 10 - fromIntegral i * 60

backdrop :: Picture
backdrop = color (makeColor 0.2 0.2 0.3 1) (rectangleWire 760 560)

controlsPicture :: Picture
controlsPicture =
    pictures
        [ backdrop
        , centered 0.35 200 yellow "Controls"
        , pictures [centered 0.17 (120 - fromIntegral i * 45) white l | (i, l) <- zip [0 :: Int ..] ls]
        , centered 0.12 (-230) (greyN 0.5) "Enter: back"
        ]
  where
    ls =
        [ "A / D : move"
        , "W : jump"
        , "S : fast fall"
        , "P : pause"
        , "B : debug hitboxes"
        , "K / L : save / load"
        ]

levelSelectPicture :: GameState -> Picture
levelSelectPicture gs =
    pictures
        [ backdrop
        , centered 0.4 130 yellow "Select Level"
        , pictures (zipWith item [0 ..] (availableLevels gs))
        , centered 0.12 (-230) (greyN 0.5) "W/S: select    Enter: play    M: back"
        ]
  where
    item :: Int -> String -> Picture
    item i name
        | i == levelIndex gs = centered 0.25 y yellow ("> " ++ name ++ " <")
        | otherwise = centered 0.2 y (greyN 0.7) name
      where
        y = 30 - fromIntegral i * 60
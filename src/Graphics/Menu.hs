module Graphics.Menu where

import Core.Constants
import Core.Types (Levels (..), PauseOption (..))
import Entities.Terrain ()
import Game.Levels (allLevels)
import Game.State
import Graphics.FontMetrics
import Graphics.Gloss

-- | Centers strings in window.
centered :: Float -> Float -> Color -> String -> Picture
centered s y c str =
    translate (negate (stringAdvance str * s) / 2) y $ scale s s $ color c $ text str

-- | Generates the main menu layout and highlighted selection options.
mainMenuPicture :: GameState -> Picture
mainMenuPicture gs =
    pictures
        [ backdrop
        , centered 0.5 130 yellow "Super Haskell"
        , centered 0.15 95 (greyN 0.6) "Made by: Pieter de Kruiff & Dimitrije Miljkovic"
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

-- | Generates the pause menu layout and highlighted selection options.
pauseMenuPicture :: PauseOption -> Picture
pauseMenuPicture selected =
    pictures
        [ backdrop
        , centered 0.4 80 yellow "Paused"
        , pictures (zipWith item [0 ..] [minBound .. maxBound])
        , centered 0.12 (-180) (greyN 0.5) "W/S: select    Enter: confirm    P: resume"
        ]
  where
    item :: Int -> PauseOption -> Picture
    item i opt
        | opt == selected = centered 0.25 y yellow ("> " ++ pauseOptionLabel opt ++ " <")
        | otherwise = centered 0.2 y (greyN 0.7) (pauseOptionLabel opt)
      where
        y = 0 - fromIntegral i * 50

backdrop :: Picture
backdrop = color (makeColor 0.2 0.2 0.3 1) (rectangleWire 760 560)

-- | Displays the help screen listing in game key bindings.
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
        , pictures (zipWith item [0 ..] allLevels)
        , centered 0.12 (-230) (greyN 0.5) "W/S: select    Enter: play    M: back"
        ]
  where
    item :: Int -> Levels -> Picture
    item idx l
        | l == level gs = centered 0.25 y yellow ("> " ++ name ++ " <")
        | otherwise = centered 0.2 y (greyN 0.7) name
      where
        name = levelTitle l
        y = 30 - fromIntegral idx * 60

levelTitle :: Levels -> String
levelTitle l = case l of
    MonadMaze -> "The Monad Maze"
    Prelude -> "The Prelude"
    IOUnderground -> "The IO Underground"

pauseOptionLabel :: PauseOption -> String
pauseOptionLabel Resume = "Resume"
pauseOptionLabel ReturnMainMenu = "Main Menu"

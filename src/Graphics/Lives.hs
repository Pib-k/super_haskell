module Graphics.Lives where

import Entities.Terrain ()
import Core.Constants
import Graphics.Gloss

livesHud :: Int -> Picture
livesHud hp =
    pictures $
        label : [slot i | i <- [0 .. maxHealth - 1]]
  where
    left = -screenHalfWidth + hudMargin
    y = screenHalfHeight - hudMargin - 20
    label = translate left y $ scale hudScale hudScale $ color white $ text "Lives:"
    slot i =
        translate (left + hudLabelWidth + fromIntegral i * hudSpacing) y $
            scale hudScale hudScale $
                color (shade i) $
                    text "(<>)"
    shade i
        | i < hp = green
        | otherwise = makeColor 0.3 0.3 0.3 0.5
module Entities.Collectible where

import Core.Classes
import Core.Types
import Entities.Terrain (Terrain (tType), drawTextAt, textLayout)
import Graphics.FontMetrics
import Graphics.Gloss
import Graphics.Gloss (translate)
import Graphics.Gloss.Data.Picture (color)

data CollectibleType = FloatingTrue | SemiGroup
    deriving (Eq, Show)

data Collectible = Collectible
    { collectPos :: Position
    , collectBox :: BoundingBox
    , collectType :: CollectibleType
    }
    deriving (Eq, Show)

instance Collidable Collectible where
    getPos = collectPos
    getBox = collectBox

instance Renderable Collectible where
    render dbg (Collectible cPos cBox cType) =
        uncurry translate cPos $
            pictures
                [ drawCollectibleType cType cBox
                , color c (uncurry rectangleWire cBox)
                ]
      where
        c = if dbg then dim white else makeColor 0.0 0.0 0.0 0.0

drawCollectibleType :: CollectibleType -> BoundingBox -> Picture
drawCollectibleType FloatingTrue (w, h) =
    color white $
        let m = charMetrics 'T'
            (s, dy) = textLayout h m
            bracketW = charAdvance m * s
         in pictures
                [ drawTextAt s (-w / 2) dy "T"
                , drawTextAt s (w / 2 - bracketW) dy "T"
                ]
drawCollectibleType SemiGroup (w, h) =
    let sym = "<>"
        m = charMetrics '>'
        (s, dy) = textLayout h m
        segW = stringAdvance sym * s
     in color green $ drawTextAt s (-segW / 2) dy sym

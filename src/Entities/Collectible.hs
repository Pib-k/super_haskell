module Entities.Collectible where

import Core.Classes
import Core.Types

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

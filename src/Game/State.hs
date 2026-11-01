{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.State where

import Core.Classes
import Data.Aeson (FromJSON, ToJSON)
import Entities.Collectible
import Entities.Enemy
import Entities.Player
import Entities.Terrain
import GHC.Generics
import Game.Animation
import Game.Random ()
import System.Random (StdGen)

data LevelState = LevelState
    { player :: Player
    , enemies :: [Enemy]
    , terrain :: [Terrain]
    , collectibles :: [Collectible]
    , animations :: [Animation]
    , bounds :: (Float, Float)
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Resetable LevelState where
    resetToDef start cur =
        cur
            { player = (resetToDef (player start) (player cur)){collectedTrues = 0}
            , enemies = enemies start
            , terrain = terrain start
            , collectibles = collectibles start
            , animations = []
            }

data AppMode = MainMenu | LevelSelection | Playing | LevelComplete | GameOver | Controls
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data GameState = GameState
    { appMode :: AppMode
    , currentLevel :: LevelState
    , availableLevels :: [String]
    , levelIndex :: Int
    , isPaused :: Bool
    , globalScore :: Int
    , highScore :: Int
    , rngSeed :: Int
    , rngState :: StdGen
    , isDebugMode :: Bool
    , menuIndex :: Int
    }
    deriving (Show, Generic, FromJSON, ToJSON)

modifyPlayer :: (Player -> Player) -> GameState -> GameState
modifyPlayer f gs = gs{currentLevel = lvl{player = f (player lvl)}, menuIndex = 0}
  where
    lvl = currentLevel gs

modifyLevel :: (LevelState -> LevelState) -> GameState -> GameState
modifyLevel f gs = gs{currentLevel = f (currentLevel gs), menuIndex = 0}

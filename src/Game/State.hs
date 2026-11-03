{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.State where

import Core.Classes
import Core.Types (Levels, PauseOption)
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

data AppMode = MainMenu | LevelSelection | Playing | Paused PauseOption | LevelComplete | GameOver | Controls
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data GameState = GameState
    { appMode :: AppMode
    , levelState :: LevelState
    , level :: Levels
    , pauseSelection :: PauseOption
    , globalScore :: Int
    , highScore :: Int
    , rngSeed :: Int
    , rngState :: StdGen
    , isDebugMode :: Bool
    , menuIndex :: Int
    }
    deriving (Show, Generic, FromJSON, ToJSON)

modifyPlayer :: (Player -> Player) -> GameState -> GameState
modifyPlayer f gs = gs{levelState = lvl{player = f (player lvl)}, menuIndex = 0}
  where
    lvl = levelState gs

modifyLevel :: (LevelState -> LevelState) -> GameState -> GameState
modifyLevel f gs = gs{levelState = f (levelState gs), menuIndex = 0}

module Game.State where

import Core.Classes
import Core.Types
import Entities.Collectible
import Entities.Enemy
import Entities.Player
import Entities.Terrain
import Game.Animation
import System.Random (StdGen)

data LevelState = LevelState
    { player :: Player
    , enemies :: [Enemy]
    , terrain :: [Terrain]
    , collectibles :: [Collectible]
    , animations :: [Animation]
    , bounds :: (Float, Float)
    }
    deriving (Eq, Show)

instance Resetable LevelState where
    resetToDef start cur =
        cur
            { player = resetToDef (player start) (player cur)
            , enemies = enemies start
            , animations = []
            }

data AppMode = MainMenu | LevelSelection | Playing
    deriving (Eq, Show)

data GameState = GameState
    { appMode :: AppMode
    , currentLevel :: LevelState
    , availableLevels :: [String]
    , levelIndex :: Int
    , isPaused :: Bool
    , globalScore :: Int
    , rngSeed :: Int
    , rngState :: StdGen
    , isDebugMode :: Bool
    }
    deriving (Show)

modifyPlayer :: (Player -> Player) -> GameState -> GameState
modifyPlayer f gs = gs{currentLevel = lvl{player = f (player lvl)}}
  where
    lvl = currentLevel gs

modifyLevel :: (LevelState -> LevelState) -> GameState -> GameState
modifyLevel f gs = gs{currentLevel = f (currentLevel gs)}

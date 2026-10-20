module Types where

import System.Random

{----- GAME STATE -----}

data LevelState = LevelState
    { player :: Player
    , enemies :: [Enemy]
    , terrain :: [Terrain]
    , collectibles :: [Collectible]
    , animations :: [Animation]
    , bounds :: (Float, Float)
    }
data GameState = GameState
    { appMode :: AppMode
    , currentLevel :: LevelState
    , availableLevels :: [String]
    , levelIndex :: Int
    , isPaused :: Bool
    , globalScore :: Int
    , rngSeed :: Int
    , rngState :: StdGen
    }

{----- PLAYER AND TERRAIN -----}

type Position = (Float, Float)
type Velocity = (Float, Float)
type BoundingBox = (Float, Float) -- (width, height)
data Collectible = Collectible
    { collectPos :: Position
    , collectBox :: BoundingBox
    , collectType :: CollectibleType
    }
data CollectibleType = FloatingTrue | SemiGroup
data Player = Player
    { playerPos :: Position
    , playerVel :: Velocity
    , playerBox :: BoundingBox
    , isGrounded :: Bool
    , health :: Int
    , score :: Int
    , collectedTrues :: Int
    }

data TerrainType = SolidList | FragileComment {opacity :: Float, isTriggered :: Bool} | BottomPit | BindTeleporter {targetPos :: Position} | ConveyorBelt {beltSpeed :: Float} | PatternGuard {isLocked :: Bool, requiredTrues :: Int} | InfiniteList {listOffset :: Float} | MainDoGoal | UnitPlatform
data Terrain = Terrain {terrainPos :: Position, terrainBox :: BoundingBox, tType :: TerrainType}

{----- ENEMIES -----}
data Enemy = Enemy
    { enemyPos :: Position
    , enemyBox :: BoundingBox
    , enemyType :: EnemyType
    , enemyVel :: Velocity
    }

data TransformerLayer = OuterMaybeT | MiddleStateT | BaseIO
data EnemyType
    = TypeMismatch {direction :: Float}
    | SpaceLeak {size :: Float}
    | MonadTransformer {layer :: TransformerLayer}
    | StrictnessBang {chargeSpeed :: Float, isCharging :: Bool}

{----- INTERFACE -----}
data AppMode = MainMenu | LevelSelection | Playing

-- ANIMATION

data AnimationType = DefeatExplosion | MainDoFountain
data Animation = Animation
    { animPos :: Position
    , animType :: AnimationType
    , animTimer :: Float
    , animDuration :: Float
    , frameIndex :: Int
    }

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
, hitbox :: BoundingBox
, isGrounded :: Bool
, health :: Int
, score :: Int
, collectedTrues :: Int
}

{----- ENEMIES -----}
data Enemy = Enemy
{ enemyPos :: Position
, enemyBox :: BoundingBox
, enemyType :: EnemyType
, velocity :: Velocity
}
data TransformerLayer = OuterMaybeT | MiddleStateT | BaseIO
data EnemyType
= TypeMismatch { direction :: Float }
| SpaceLeak { size :: Float }
| MonadTransformer { layer :: TransformerLayer }
| StrictnessBang { chargeSpeed :: Float, isCharging :: Bool }

{----- INTERFACE -----}
data AppMode = MainMenu | LevelSelection | Playing
initialState :: IO GameState
viewWorld :: GameState -> IO Picture
handleInput :: Event -> GameState -> IO GameState
stepWorld :: Float -> GameState -> IO GameState
viewPure :: GameState -> Picture
handleInputPure :: Event -> GameState -> GameState
stepPure :: Float -> GameState -> GameState

-- ANIMATION

data AnimationType = DefeatExplosion | MainDoFountain
data Animation = Animation
{ animPos :: Position
, animType :: AnimationType
, animTimer :: Float
, animDuration :: Float
, frameIndex :: Int
}
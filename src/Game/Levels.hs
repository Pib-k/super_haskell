module Game.Levels where

import Entities.Collectible
import Entities.Enemy
import Entities.Player
import Entities.Terrain
import Game.State

testTerrain :: [Terrain]
testTerrain =
    [ mkTerrain (0, -290) (800, 30) BottomPit
    , mkTerrain (0, -240) (800, 30) SolidList
    , mkTerrain (-200, -160) (260, 30) (ConveyorBelt 200.0)
    , mkTerrain (200, -160) (260, 30) (FragileComment 0.8 False)
    , mkTerrain (-250, -60) (90, 30) UnitPlatform
    , mkTerrain (0, 0) (30, 90) (PatternGuard True 3)
    , mkTerrain (250, -60) (180, 30) (InfiniteList 0.0 100.0 50.0)
    , mkTerrain (-180, 60) (100, 30) (BindTeleporter (180, 60))
    , mkTerrain (180, -30) (50, 30) MainDoGoal
    ]

testEnemy :: [Enemy]
testEnemy =
    [ Enemy (-40, -65) (30, 30) (TypeMismatch 1) (0, 0) False
    , Enemy (-150, 10) (30, 30) (SpaceLeak 5) (0, 0) False
    , Enemy (150, -100) (30, 30) (StrictnessBang IdleThunk) (0, 0) False
    , Enemy (150, -150) (layerBox OuterMaybeT) (MonadTransformer OuterMaybeT) (0, 0) False
    ]

testLevel :: LevelState
testLevel =
    LevelState
        { player =
            Player
                { playerPos = (-85, 0)
                , playerVel = (0, 0)
                , playerBox = (30, 30)
                , isGrounded = False
                , health = 3
                , score = 0
                , collectedTrues = 0
                }
        , enemies = testEnemy
        , terrain = testTerrain
        , collectibles = []
        , animations = []
        , bounds = (800, 600)
        }

thePrelude :: LevelState
thePrelude =
    LevelState
        { player =
            Player
                { playerPos = (-300, 0)
                , playerVel = (0, 0)
                , playerBox = (30, 40)
                , isGrounded = False
                , health = 3
                , score = 0
                , collectedTrues = 0
                }
        , enemies =
            [ Enemy (0, -200) (30, 30) (TypeMismatch 1) (0, 0) False
            ]
        , terrain =
            [ mkTerrain (0, -290) (800, 30) BottomPit
            , mkTerrain (-300, -60) (150, 30) SolidList
            , mkTerrain (-120, -100) (140, 30) (FragileComment 1.0 False)
            , mkTerrain (80, -120) (180, 30) (ConveyorBelt 150.0)
            , mkTerrain (0, -230) (600, 30) SolidList
            , mkTerrain (0, 80) (80, 30) UnitPlatform
            , mkTerrain (-300, 100) (60, 30) UnitPlatform
            , mkTerrain (220, -180) (30, 80) (PatternGuard True 3)
            , mkTerrain (320, -180) (140, 30) MainDoGoal
            ]
        , collectibles =
            [ Collectible (-120, -60) (20, 20) FloatingTrue
            , Collectible (80, -80) (20, 20) FloatingTrue
            , Collectible (0, 120) (20, 20) FloatingTrue
            , Collectible (-300, 140) (20, 20) SemiGroup
            ]
        , animations = []
        , bounds = (800, 600)
        }

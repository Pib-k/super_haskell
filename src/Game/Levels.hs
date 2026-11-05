module Game.Levels where

import Core.Constants
import Core.Types (Levels (..), Position)
import Entities.Collectible
import Entities.Enemy
import Entities.Player
import Entities.Terrain
import Game.State

-- | A level section containing terrain, enemies and items to simplify level creation.
data Section = Section
    { secTerrain :: [Terrain]
    , secEnemies :: [Enemy]
    , secItems :: [Collectible]
    }

levelFor :: Levels -> LevelState
levelFor l = case l of
    Prelude -> thePrelude
    MonadMaze -> theMonadMaze
    IOUnderground -> theIOUnderground

allLevels :: [Levels]
allLevels = [minBound .. maxBound]

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

-- | The test level for development and debugging
testLevel :: LevelState
testLevel =
    LevelState
        { player =
            Player
                { playerPos = (-85, 0)
                , playerVel = (0, 0)
                , playerBox = levelPlayerBox
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

newPlayer :: Position -> Player
newPlayer pos =
    Player
        { playerPos = pos
        , playerVel = (0, 0)
        , playerBox = levelPlayerBox
        , isGrounded = False
        , health = 3
        , score = 0
        , collectedTrues = 0
        }

-- | Functions for creating game entities to make level design easier
enemy :: Position -> EnemyType -> Enemy
enemy pos ty = Enemy pos (30, 30) ty (0, 0) False

shiftTerrain :: Float -> Terrain -> Terrain
shiftTerrain dx t = t{terrainPos = (x + dx, y), tType = shiftType (tType t)}
  where
    (x, y) = terrainPos t
    shiftType (BindTeleporter (tx, ty)) = BindTeleporter (tx + dx, ty)
    shiftType tt = tt

shiftEnemy :: Float -> Enemy -> Enemy
shiftEnemy dx e = e{enemyPos = (fst (enemyPos e) + dx, snd (enemyPos e))}

shiftItem :: Float -> Collectible -> Collectible
shiftItem dx c = c{collectPos = (fst (collectPos c) + dx, snd (collectPos c))}

buildLevel :: Position -> [Section] -> LevelState
buildLevel (sx, sy) secs =
    LevelState
        { player = newPlayer (x0 + sx, sy)
        , enemies = concat [map (shiftEnemy dx) (secEnemies s) | (dx, s) <- placed]
        , terrain = pit : concat [map (shiftTerrain dx) (secTerrain s) | (dx, s) <- placed]
        , collectibles = concat [map (shiftItem dx) (secItems s) | (dx, s) <- placed]
        , animations = []
        , bounds = (totalW, 600)
        }
  where
    totalW = sectionWidth * fromIntegral (length secs)
    x0 = negate totalW / 2
    placed = zip [x0 + sectionWidth * fromIntegral i | i <- [0 :: Int ..]] secs
    pit = mkTerrain (0, -290) (totalW, 30) BottomPit

groundTop :: Float
groundTop = -225

flag :: Float -> Float -> Terrain
flag x top = mkTerrain (x, top + flagHeight / 2) (flagThickness, flagHeight) MainDoGoal

ground :: Float -> Float -> Terrain
ground x w = mkTerrain (x, -240) (w, 30) SolidList

ledge :: Float -> Float -> Float -> Terrain
ledge x y w = mkTerrain (x, y) (w, 30) SolidList

stone :: Float -> Float -> Terrain
stone x y = mkTerrain (x, y) (40, 30) UnitPlatform

ride :: Float -> Float -> Float -> Float -> Terrain
ride x y reach v = mkTerrain (x, y) (70, 30) (InfiniteList 0 reach v)

item :: Position -> CollectibleType -> Collectible
item pos = Collectible pos collectibleSize

-- | Level designs for 'The Prelude', 'The Monad Maze', and 'The IO Underground'
thePrelude :: LevelState
thePrelude = buildLevel (80, -190) [p0, p1, p2, p3, p4]
  where
    p0 = Section [ground 400 800] [enemy (550, -210) (TypeMismatch 1)] []
    p1 =
        Section
            [ground 130 260, stone 330 (-180), stone 460 (-150), stone 570 (-120), ground 700 200]
            [enemy (720, -210) (TypeMismatch (-1))]
            []
    p2 =
        Section
            [ ground 100 200
            , mkTerrain (300, -240) (200, 30) (FragileComment 1.0 False)
            , ground 600 400
            , ledge 550 (-165) 120
            ]
            [enemy (700, -210) (TypeMismatch 1)]
            [item (550, -135) SemiGroup]
    p3 =
        Section
            [ground 75 150, mkTerrain (300, -240) (300, 30) (ConveyorBelt (-100)), ground 625 350]
            [enemy (600, -210) (TypeMismatch 1), enemy (750, -210) (TypeMismatch (-1))]
            []
    p4 =
        Section
            [ground 400 800, ledge 250 (-170) 100, ledge 420 (-115) 100, flag 720 groundTop]
            [enemy (150, -210) (TypeMismatch 1), enemy (560, -210) (TypeMismatch (-1))]
            []

theMonadMaze :: LevelState
theMonadMaze = buildLevel (80, -190) [m0, m1, m2, m3, m4]
  where
    m0 =
        Section
            [ ground 400 800
            , ledge 400 (-165) 120
            , mkTerrain (650, -210) (60, 30) (BindTeleporter (150, 175))
            , ledge 150 130 120
            ]
            []
            [item (400, -135) FloatingTrue, item (190, 160) FloatingTrue]
    m1 =
        Section
            [ground 400 800, mkTerrain (400, -150) (30, 150) (PatternGuard True 2)]
            [ enemy (150, -210) (SpaceLeak 5)
            , Enemy (100, -210) (layerBox OuterMaybeT) (MonadTransformer OuterMaybeT) (0, 0) False
            ]
            []
    m2 =
        Section
            [ground 400 800, mkTerrain (400, -165) (240, 30) (ConveyorBelt (-80))]
            [enemy (150, -210) (SpaceLeak 5), enemy (700, -210) (SpaceLeak 5)]
            [item (400, -135) FloatingTrue]
    m3 =
        Section
            [ ground 100 200
            , mkTerrain (60, -150) (30, 150) (PatternGuard True 3)
            , stone 330 (-180)
            , stone 460 (-150)
            , stone 590 (-180)
            , ground 740 120
            ]
            [enemy (740, -210) (SpaceLeak 5)]
            [item (460, -120) SemiGroup]
    m4 =
        Section
            [ground 400 800, flag 740 groundTop]
            [ enemy (300, -210) (SpaceLeak 5)
            ]
            []

theIOUnderground :: LevelState
theIOUnderground = buildLevel (80, -160) [i0, i1, i2, i3, i4, i5]
  where
    bang x y = enemy (x, y) (StrictnessBang IdleThunk)
    i0 =
        Section
            [ledge 100 (-200) 200, ride 300 (-200) 120 60, ride 500 (-190) 100 70, ledge 730 (-200) 140]
            [bang 730 (-120)]
            []
    i1 =
        Section
            [ ledge 100 (-200) 200
            , stone 260 (-150)
            , stone 380 (-90)
            , stone 500 (-30)
            , stone 620 (-90)
            , stone 730 (-150)
            ]
            []
            [item (500, 0) SemiGroup]
    i2 =
        Section
            [ledge 100 (-200) 200, ride 300 (-190) 140 90, ride 520 (-170) 100 80, ledge 740 (-200) 120]
            [bang 740 (-120)]
            []
    i3 = Section [ledge 400 (-200) 800] [bang 250 (-120), bang 500 (-140), bang 720 (-90)] []
    i4 =
        Section
            [ledge 60 (-200) 120, ride 200 (-200) 150 100, ride 440 (-170) 120 90, ride 660 (-200) 60 80]
            []
            []
    i5 = Section [ledge 400 (-200) 800, flag 700 (-185)] [bang 300 (-120)] []

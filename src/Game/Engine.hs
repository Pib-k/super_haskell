module Game.Engine where

import Core.Classes
import Core.Constants
import Core.Types
import Entities.Enemy
import Entities.Player
import Entities.Terrain
import Game.State

testTerrain :: [Terrain]
testTerrain =
    [ mkTerrain (0, -290) (800, 30) BottomPit
    , mkTerrain (0, -240) (800, 30) SolidList
    , mkTerrain (-200, -160) (260, 30) (ConveyorBelt 50.0)
    , mkTerrain (200, -160) (260, 30) (FragileComment 0.8 False)
    , mkTerrain (-250, -60) (90, 30) UnitPlatform
    , mkTerrain (0, 0) (30, 90) (PatternGuard True 3)
    , mkTerrain (250, -60) (180, 30) (InfiniteList 0.0 100.0 50.0)
    , mkTerrain (-180, 60) (100, 30) (BindTeleporter (180, 60))
    , mkTerrain (180, 60) (180, 30) MainDoGoal
    ]

testLevel :: LevelState
testLevel =
    LevelState
        { player =
            Player
                { playerPos = (-85, 0)
                , playerVel = (0, 0)
                , playerBox = (30, 40)
                , isGrounded = False
                , health = 3
                , score = 0
                , collectedTrues = 0
                }
        , enemies = [Enemy (-40, -65) (30, 30) (TypeMismatch 1) (0, 0) False]
        , terrain = testTerrain
        , collectibles = []
        , animations = []
        , bounds = (800, 600)
        }

stepPure :: Float -> GameState -> GameState
stepPure dt gs = case appMode gs of
    Playing | not (isPaused gs) -> gameOverCheck (respawnCheck gs (modifyLevel (stepLevel step) gs))
    _ -> gs
  where
    step = min dt maxStep

stepLevel :: Float -> LevelState -> LevelState
stepLevel dt lvl =
    handleContacts
        lvl
            { terrain = newTerrain
            , player = stepPlayer dt newTerrain (player lvl)
            , enemies = map (stepEnemy dt newTerrain) (enemies lvl)
            }
  where
    newTerrain = map (stepTerrain dt) (terrain lvl)

respawnCheck :: GameState -> GameState -> GameState
respawnCheck before after
    | lives after < lives before = modifyLevel (resetToDef testLevel) after
    | otherwise = after
  where
    lives = health . player . currentLevel

gameOverCheck :: GameState -> GameState
gameOverCheck gs
    | health (player (currentLevel gs)) <= 0 =
        gs{appMode = MainMenu, currentLevel = testLevel}
    | otherwise = gs

handleContacts :: LevelState -> LevelState
handleContacts lvl =
    lvl
        { player = p'
        , enemies = survivors
        , animations = map explosion stomped ++ animations lvl
        }
  where
    p = player lvl
    hits = [(e, checkCollision p e) | e <- enemies lvl]
    stomped = [e | (e, Just FromBottom) <- hits]
    hurtBy = [e | (e, Just d) <- hits, d /= FromBottom]
    survivors = [e | (e, c) <- hits, c /= Just FromBottom]
    p'
        | not (null stomped) = bounce p{score = score p + stompScore * length stomped}
        | not (null hurtBy) = hurt p
        | otherwise = p

explosion :: Enemy -> Animation
explosion e =
    Animation
        { animPos = getPos e
        , animType = DefeatExplosion
        , animTimer = 0
        , animDuration = explosionDuration
        , frameIndex = 0
        }

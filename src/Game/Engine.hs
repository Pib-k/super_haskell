module Game.Engine where

import Core.Classes
import Core.Constants
import Core.Types
import Data.List (partition)
import Data.Maybe (isJust, isNothing)
import Entities.Collectible
import Entities.Enemy
import Entities.Player
import Entities.Terrain
import Game.Levels
import Game.State
import Game.Animation

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
            , animations = updateAnimation dt (animations lvl)
            }
  where
    newTerrain = map (stepTerrain dt) (terrain lvl)
    pPos = playerPos (player lvl)

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
        { player = foldl' (flip applyItem) p' pickedUp
        , enemies = survivors ++ transformed
        , animations = map explosion deadEnemies ++ animations lvl
        , terrain = map (triggerTerrain p') ts
        , collectibles = remaining
        }
  where
    p = player lvl
    ts = terrain lvl
    hits = [(e, checkCollision p e) | e <- enemies lvl]
    stompedHits = [e | (e, Just FromBottom) <- hits]
    hurtBy = [e | (e, Just d) <- hits, d /= FromBottom]
    survivors = [e | (e, c) <- hits, c /= Just FromBottom]
    stompResults = [(e, stompEnemy e) | e <- stompedHits]
    deadEnemies = [e | (e, Nothing) <- stompResults]
    transformed = [newE | (_, Just newE) <- stompResults]
    (pickedUp, remaining) = partition (\c -> isJust (checkCollision p c)) $ collectibles lvl
    p'
        | not (null stompedHits) = bounce p{score = score p + stompScore * length stompedHits}
        | not (null hurtBy) = hurt p
        | otherwise = p

applyItem :: Collectible -> Player -> Player
applyItem c p = case collectType c of
    FloatingTrue ->
        p
            { collectedTrues = collectedTrues p + 1
            , score = score p + 50
            }
    SemiGroup -> p{health = health p + 1}

triggerTerrain :: Player -> Terrain -> Terrain
triggerTerrain p t = case tType t of
    FragileComment opac False
        | checkCollision p t == Just FromBottom -> t{tType = FragileComment opac True}
    PatternGuard True req
        | collectedTrues p >= req -> t{tType = PatternGuard False req}
    _ -> t
    
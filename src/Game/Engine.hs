module Game.Engine where

import Core.Classes
import Core.Constants
import Core.Types
import Data.List (partition)
import Data.Maybe (isJust)
import Entities.Collectible
import Entities.Enemy
import Entities.Player
import Entities.Terrain
import Game.Animation
import Game.Levels
import Game.State

stepPure :: Float -> GameState -> GameState
stepPure dt gs = case appMode gs of
    Playing -> finishCheck step $ updateHighScore $ respawnCheck gs $ gameOverCheck $ modifyLevel (stepLevel step (rngSeed gs)) gs
    _ -> gs
  where
    step = min dt maxStep

stepLevel :: Float -> Int -> LevelState -> LevelState
stepLevel dt s lvl =
    handleContacts
        lvl
            { terrain = newTerrain
            , player = stepPlayer dt newTerrain (player lvl)
            , enemies = map (stepEnemy dt pPos newTerrain) (enemies lvl)
            , animations = updateAnimation dt (animations lvl)
            }
        s
  where
    newTerrain = map (stepTerrain dt) (terrain lvl)
    pPos = playerPos (player lvl)

finishCheck :: Float -> GameState -> GameState
finishCheck step gs
    | appMode gs == Playing && fountainEnds =
        if level gs == maxBound
            then gs{appMode = GameOver Victory}
            else gs{appMode = LevelComplete}
    | otherwise = gs
  where
    fountainEnds =
        any
            (\a -> animType a == MainDoFountain && animTimer a + step >= animDuration a)
            (animations (levelState gs))

respawnCheck :: GameState -> GameState -> GameState
respawnCheck before after
    | appMode after /= Playing = after
    | lives after < lives before = modifyLevel (resetToDef (levelFor (level after))) after
    | otherwise = after
  where
    lives = health . player . levelState

gameOverCheck :: GameState -> GameState
gameOverCheck gs
    | inPit lvl = gs{appMode = GameOver RuntimeException}
    | health (player lvl) <= 0 = gs{appMode = GameOver Normal}
    | otherwise = gs
  where
    lvl = levelState gs
    p = player lvl
    inPit l =
        snd (playerPos p) < voidY
            || any (\t -> tType t == BottomPit && isJust (checkCollision p t)) (terrain l)

handleContacts :: LevelState -> Int -> LevelState
handleContacts lvl s =
    handleGoal s
        . handleTerrain
        . handleCollectibles
        . handleEnemies
        $ lvl

handleEnemies :: LevelState -> LevelState
handleEnemies lvl =
    lvl
        { player = p'
        , enemies = survivors ++ transformed
        , animations = map explosion deadEnemies ++ animations lvl
        , collectibles = map dropCollectible deadEnemies ++ collectibles lvl
        }
  where
    p = player lvl
    hits = [(e, checkCollision p e) | e <- enemies lvl]
    stompedHits = [e | (e, Just FromBottom) <- hits]
    hurtBy = [e | (e, Just d) <- hits, d /= FromBottom]
    survivors = [e | (e, c) <- hits, c /= Just FromBottom]
    stompResults = [(e, stompEnemy e) | e <- stompedHits]
    deadEnemies = [e | (e, Nothing) <- stompResults]
    transformed = [newE | (_, Just newE) <- stompResults]

    p'
        | not (null stompedHits) = bounce p{score = score p + stompScore * length stompedHits}
        | not (null hurtBy) = hurt p
        | otherwise = p

handleTerrain :: LevelState -> LevelState
handleTerrain lvl =
    lvl{terrain = map (triggerTerrain (player lvl)) (terrain lvl)}

handleCollectibles :: LevelState -> LevelState
handleCollectibles lvl =
    lvl
        { player = foldl' (flip applyItem) (player lvl) pickedUp
        , collectibles = remaining
        }
  where
    p = player lvl
    (pickedUp, remaining) = partition (\c -> isJust (checkCollision p c)) (collectibles lvl)

handleGoal :: Int -> LevelState -> LevelState
handleGoal s lvl =
    case [t | t <- terrain lvl, tType t == MainDoGoal, isJust (checkCollision (player lvl) t)] of
        (goal : _)
            | not hasFountain ->
                lvl{animations = mainDoFountain (getPos goal) s : animations lvl}
        _ -> lvl
  where
    hasFountain = any (\a -> animType a == MainDoFountain) (animations lvl)

dropCollectible :: Enemy -> Collectible
dropCollectible e =
    Collectible
        { collectPos = getPos e
        , collectBox = collectibleSize
        , collectType = SemiGroup
        }

applyItem :: Collectible -> Player -> Player
applyItem c p = case collectType c of
    FloatingTrue ->
        p
            { collectedTrues = collectedTrues p + 1
            , score = score p + 50
            }
    SemiGroup
        | health p < maxHealth -> p{health = health p + 1}
        | otherwise -> p{score = score p + collectibleScore}

triggerTerrain :: Player -> Terrain -> Terrain
triggerTerrain p t = case tType t of
    FragileComment opac False
        | checkCollision p t == Just FromBottom -> t{tType = FragileComment opac True}
    PatternGuard True req
        | collectedTrues p >= req -> t{tType = PatternGuard False req}
    _ -> t

updateHighScore :: GameState -> GameState
updateHighScore gs =
    let currentScore = score (player (levelState gs))
     in gs{highScore = max (highScore gs) currentScore}

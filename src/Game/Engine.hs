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
    Playing -> finishCheck $ updateHighScore $ gameOverCheck (respawnCheck gs (modifyLevel (stepLevel step (rngSeed gs)) gs))
    _ -> gs
  where
    step = min dt maxStep
    finishCheck g
        | appMode g == Playing && fountainEnds = g{appMode = LevelComplete}
        | otherwise = g
    fountainEnds =
        any
            (\a -> animType a == MainDoFountain && animTimer a + step >= animDuration a)
            (animations (levelState gs))

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

respawnCheck :: GameState -> GameState -> GameState
respawnCheck before after
    | lives after < lives before = modifyLevel (resetToDef (levelFor (level after))) after
    | otherwise = after
  where
    lives = health . player . levelState

gameOverCheck :: GameState -> GameState
gameOverCheck gs
    | health (player (levelState gs)) <= 0 = gs{appMode = GameOver}
    | otherwise = gs

handleContacts :: LevelState -> Int -> LevelState
handleContacts lvl s =
    lvl
        { player = foldl' (flip applyItem) p' pickedUp
        , enemies = survivors ++ transformed
        , animations = newGoalAnims ++ map explosion deadEnemies ++ animations lvl
        , terrain = map (triggerTerrain p') ts
        , collectibles = map dropCollectible deadEnemies ++ remaining
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
    goalHits = [t | t <- ts, tType t == MainDoGoal, isJust (checkCollision p t)]
    hasFountain = any (\a -> animType a == MainDoFountain) (animations lvl)
    newGoalAnims
        | not (null goalHits) && not hasFountain = [mainDoFountain (getPos (head goalHits)) s]
        | otherwise = []

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

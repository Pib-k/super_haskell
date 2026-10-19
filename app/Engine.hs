module Engine where

import Classes
import Data.Char (toLower)
import Graphics.Gloss.Interface.IO.Game
import System.Random (getStdGen)
import Types
import Types (GameState)

moveSpeed, jumpSpeed, fastFallSpeed :: Float
moveSpeed = 200
jumpSpeed = 400
fastFallSpeed = -300

handleInputPure :: Event -> GameState -> GameState
handleInputPure (EventKey key keyState _ _) gs =
    case appMode gs of
        MainMenu -> menuKey k keyState gs
        LevelSelection -> selectionKey k keyState gs
        Playing -> playingKey k keyState gs
  where
    k = normalize key
handleInputPure _ gs = gs

normalize :: Key -> Key
normalize (Char c) = Char (toLower c)
normalize k = k

menuKey :: Key -> KeyState -> GameState -> GameState
menuKey (SpecialKey KeyEnter) Down gs = gs{appMode = LevelSelection}
menuKey _ _ gs = gs

selectionKey :: Key -> KeyState -> GameState -> GameState
selectionKey key Down gs = case key of
    Char 'a' -> gs{levelIndex = max 0 (levelIndex gs - 1)}
    Char 'd' -> gs{levelIndex = min lastIdx (levelIndex gs + 1)}
    SpecialKey KeyEnter -> gs{appMode = Playing}
    _ -> gs
  where
    lastIdx = length (availableLevels gs) - 1
selectionKey _ _ gs = gs

playingKey :: Key -> KeyState -> GameState -> GameState
playingKey (Char 'p') Down gs = gs{isPaused = not (isPaused gs)}
playingKey key keyState gs
    | isPaused gs = gs
    | otherwise = modifyPlayer (playerKey key keyState) gs

playerKey :: Key -> KeyState -> Player -> Player
playerKey (Char 'a') Down p = setVx (-moveSpeed) p
playerKey (Char 'a') Up p = if fst (getVel p) < 0 then setVx 0 p else p
playerKey (Char 'd') Down p = setVx moveSpeed p
playerKey (Char 'd') Up p = if fst (getVel p) > 0 then setVx 0 p else p
playerKey (Char 'w') Down p
    | isGrounded p = setVy jumpSpeed p
playerKey (Char 's') Down p = setVy fastFallSpeed p
playerKey _ _ p = p

setVx, setVy :: Float -> Player -> Player
setVx x p = setVel (x, snd (getVel p)) p
setVy y p = setVel (fst (getVel p), y) p

modifyPlayer :: (Player -> Player) -> GameState -> GameState
modifyPlayer f gs = gs{currentLevel = lvl{player = f (player lvl)}}
  where
    lvl = currentLevel gs

gravity :: Float
gravity = -900

initialState :: IO GameState
initialState = do
    gen <- getStdGen
    pure
        GameState
            { appMode = MainMenu
            , currentLevel = testLevel
            , availableLevels = ["The Prelude", "The Monad Maze", "The IO Underground"]
            , levelIndex = 0
            , isPaused = False
            , globalScore = 0
            , rngSeed = 0
            , rngState = gen
            }

testLevel :: LevelState
testLevel =
    LevelState
        { player =
            Player
                { playerPos = (0, 0)
                , playerVel = (0, 0)
                , playerBox = (30, 40)
                , isGrounded = False
                , health = 3
                , score = 0
                , collectedTrues = 0
                }
        , enemies = []
        , terrain = [Terrain (0, -250) (800, 40) SolidList, Terrain (0, -100) (300, 40) SolidList]
        , collectibles = []
        , animations = []
        , bounds = (800, 600)
        }

stepPure :: Float -> GameState -> GameState
stepPure dt gs = case appMode gs of
    Playing | not (isPaused gs) -> modifyPlayer (stepPlayer dt (terrain (currentLevel gs))) gs
    _ -> gs

stepPlayer :: Float -> [Terrain] -> Player -> Player
stepPlayer dt ts p = foldl resolve moved ts
  where
    (vx, vy) = getVel p
    falling = setVel (vx, vy + gravity * dt) p
    moved = (applyVelocity dt falling){isGrounded = False}

resolve :: Player -> Terrain -> Player
resolve p t = case checkCollision p t of
    Just FromBottom ->
        let newY = ty + (th + ph) / 2
         in (setPos (px, newY) (setVel (vx, 0) p)){isGrounded = True}
    Just FromTop ->
        let newY = ty - (th + ph) / 2
         in setPos (px, newY) (setVel (vx, -1) p)
    Just FromLeft ->
        let newX = tx + (tw + pw) / 2
         in setPos (newX, py) (setVel (0, vy) p)
    Just FromRight ->
        let newX = tx - (tw + pw) / 2
         in setPos (newX, py) (setVel (0, vy) p)
    _ -> p
  where
    (px, py) = getPos p
    (tx, ty) = getPos t
    (tw, th) = getBox t
    (pw, ph) = getBox p
    (vx, vy) = getVel p

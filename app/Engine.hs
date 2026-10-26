module Engine where

import Classes
import Data.Char (toLower)
import Graphics.Gloss.Interface.IO.Game
import System.Random (getStdGen)
import Terrain (mkTerrain, stepTerrain)
import Terrain (mkTerrain)
import Data.Maybe (isJust)
import Types
import GHC.ForeignPtr (addForeignPtrConcFinalizer)

{----- CONSTANTS -----}

moveSpeed, jumpSpeed, fastFallSpeed :: Float
moveSpeed = 200
jumpSpeed = 400
fastFallSpeed = -300

gravity :: Float
gravity = -900

voidY :: Float
voidY = -400

maxStep :: Float
maxStep = 1 / 60

walkSpeed :: Float
walkSpeed = 60

bounceSpeed, explosionDuration :: Float
bounceSpeed = 300
explosionDuration = 0.5

stompScore :: Int
stompScore = 100

spawnPosition :: Position
spawnPosition = (0, 0)

{----- INPUT -----}

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
playingKey (Char 'b') Down gs = gs{isDebugMode = not (isDebugMode gs)}
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

{----- HELPER FUNC. FOR STATE -----}

setVx, setVy :: Float -> Player -> Player
setVx x p = setVel (x, snd (getVel p)) p
setVy y p = setVel (fst (getVel p), y) p

modifyPlayer :: (Player -> Player) -> GameState -> GameState
modifyPlayer f gs = gs{currentLevel = lvl{player = f (player lvl)}}
  where
    lvl = currentLevel gs

modifyLevel :: (LevelState -> LevelState) -> GameState -> GameState
modifyLevel f gs = gs{currentLevel = f (currentLevel gs)}

stepLevel :: Float -> LevelState -> LevelState
stepLevel dt lvl =
    lvl
        { terrain = newTerrain
        , player = stepPlayer dt newTerrain (player lvl)
        }
  where
    newTerrain = map (stepTerrain dt) (terrain lvl)

gravity :: Float
gravity = -900

{----- BEGIN STATE AND LEVEL -----}
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
            , isDebugMode = False
            }

testTerrain :: [Terrain]
testTerrain =
    [ mkTerrain (0, -290) (800, 30) BottomPit
    , mkTerrain (0, -240) (800, 30) SolidList
    , mkTerrain (-200, -160) (260, 30) (ConveyorBelt 50.0)
    , mkTerrain (200, -160) (260, 30) (FragileComment 0.8 False)
    , mkTerrain (-250, -60) (90, 30) UnitPlatform
    , mkTerrain (0, -60) (30, 90) (PatternGuard True 3) -- Tall vertical guard!
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
        , enemies = [Enemy (0, -65) (30, 30) (TypeMismatch 1) (0, 0)]
        , terrain = testTerrain
        , enemies = [Enemy (0, -65) (30, 30) (TypeMismatch 1) (0, 0)]
        , collectibles = []
        , animations = []
        , bounds = (800, 600)
        }

{----- STEP -----}

stepPure :: Float -> GameState -> GameState
stepPure dt gs = case appMode gs of
    Playing | not (isPaused gs) -> modifyLevel (stepLevel dt) gs
    _ -> gs

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

stepLevel :: Float -> LevelState -> LevelState
stepLevel dt lvl = handleContacts lvl {
    player = stepPlayer dt ts (player lvl),
    enemies = map (stepEnemy dt ts) (enemies lvl)
    }
    where
        ts = terrain lvl

hurt :: Player -> Player
hurt p = setVel (0, 0) (setPos spawnPosition p{health = health p - 1})

{----- PLAYER collission TERRAIN -----}

stepPlayer :: Float -> [Terrain] -> Player -> Player
stepPlayer dt ts p = voidCheck (foldl resolvePlayer moved ts)
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

resolvePlayer :: Player -> Terrain -> Player
resolvePlayer p t = case tType t of
    BottomPit | touching -> hurt p
    BindTeleporter target | touching -> setPos target p
    tt  | isSolid tt -> resolve p t
        | otherwise -> p
    where 
        touching = isJust (checkCollision p t)

isSolid :: TerrainType -> Bool
isSolid SolidList = True
isSolid FragileComment{} = True
isSolid ConveyorBelt{} = True
isSolid UnitPlatform = True
isSolid InfiniteList{} = True
isSolid (PatternGuard locked _) = locked
isSolid _ = False

voidCheck :: Player -> Player
voidCheck p
    | snd (getPos p) < voidY = hurt p
    | otherwise = p

{----- ENEMIES -----}

stepEnemy :: Float -> [Terrain] -> Enemy -> Enemy
stepEnemy dt ts e = case enemyType e of
    TypeMismatch dir -> walk dt ts dir e
    _ -> e

-- walking step voor TypeMismatch (later veranderen naar stepTypeMisMatch)
walk :: Float -> [Terrain] -> Float -> Enemy -> Enemy
walk dt ts dir e
    | blocked || not groundAhead = e{enemyType = TypeMismatch (-dir)}
    | otherwise = next
  where
    next = applyVelocity dt (setVel (dir * walkSpeed, 0) e)
    blocked = any (isJust . checkCollision next) ts
    (x, y) = getPos next
    (w, h) = getBox next
    probe = (x + dir * w / 2, y - h / 2 - 1)
    groundAhead = any (contains probe) ts

contains :: Position -> Terrain -> Bool
contains (px, py) t = abs (px - tx) * 2 <= tw && abs (py - ty) * 2 <= th
  where
    (tx, ty) = getPos t
    (tw, th) = getBox t

{----- PLAYER vs. ENEMIES -----}

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

bounce :: Player -> Player
bounce p = setVel (fst (getVel p), bounceSpeed) p

explosion :: Enemy -> Animation
explosion e =
    Animation
        { animPos = getPos e
        , animType = DefeatExplosion
        , animTimer = 0
        , animDuration = explosionDuration
        , frameIndex = 0
        }

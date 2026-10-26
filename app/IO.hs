{-# LANGUAGE ScopedTypeVariables #-}
module IO where

import Engine
import Control.Exception (IOException, evaluate, try)
import Graphics.Gloss (Picture)
import Text.Read (readMaybe)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..))
import Types
import Graphics.Gloss.Interface.IO.Game (Event)
import Render

{----- GLOSS -----}

viewWorld :: GameState -> IO Picture
viewWorld = pure . viewPure

stepWorld :: Float -> GameState -> IO GameState
stepWorld dt = pure . stepPure dt

handleInput :: Event -> GameState -> IO GameState
handleInput ev@(EventKey key Down _ _) gs
    | k == Char 'k' = saveProgress saveFile gs >> pure gs
    | k == Char 'l' = maybe gs (`applySave` gs) <$> loadSave saveFile
    | otherwise = pure (handleInputPure ev gs)
  where
    k = normalize key
handleInput ev gs = pure (handleInputPure ev gs)


{----- LOAD and SAVE -----}

saveFile :: FilePath
saveFile = "save.txt"

data SaveData = SaveData
    { 
        savedLevel :: Int
    ,   savedGlobalScore :: Int
    ,   savedScore :: Int
    ,   savedHealth :: Int
    ,   savedTrues :: Int
    }
    deriving (Show, Read)

toSave :: GameState -> SaveData
toSave gs =
    SaveData
        { 
            savedLevel = levelIndex gs
        ,   savedGlobalScore = globalScore gs
        ,   savedScore = score p
        ,   savedHealth = health p
        ,   savedTrues = collectedTrues p
        }
  where
    p = player (currentLevel gs)

applySave :: SaveData -> GameState -> GameState
applySave sd gs =
    modifyPlayer restore gs{levelIndex = savedLevel sd, globalScore = savedGlobalScore sd, appMode = Playing}
  where
    restore p = p
            { 
                score = savedScore sd
            ,   health = savedHealth sd
            ,   collectedTrues = savedTrues sd
            }

saveProgress :: FilePath -> GameState -> IO ()
saveProgress path = writeFile path . show . toSave

loadSave :: FilePath -> IO (Maybe SaveData)
loadSave path = do
    result <- try (readFile path >>= \s -> evaluate (length s) >> pure s)
    pure $ case result of
        Left (_ :: IOException) -> Nothing
        Right contents -> readMaybe contents

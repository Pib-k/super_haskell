{-# LANGUAGE ScopedTypeVariables #-}
module Storage.Save where

import Control.Exception (IOException, evaluate, try)
import Entities.Player
import Game.State
import Text.Read (readMaybe)

saveFile :: FilePath
saveFile = "save.txt"

data SaveData = SaveData
    { savedLevel :: Int
    , savedGlobalScore :: Int
    , savedScore :: Int
    , savedHealth :: Int
    , savedTrues :: Int
    }
    deriving (Show, Read)

toSave :: GameState -> SaveData
toSave gs =
    SaveData
        { savedLevel = levelIndex gs
        , savedGlobalScore = globalScore gs
        , savedScore = score p
        , savedHealth = health p
        , savedTrues = collectedTrues p
        }
  where
    p = player (currentLevel gs)

applySave :: SaveData -> GameState -> GameState
applySave sd gs =
    modifyPlayer restore gs{levelIndex = savedLevel sd, globalScore = savedGlobalScore sd, appMode = Playing}
  where
    restore p =
        p
            { score = savedScore sd
            , health = savedHealth sd
            , collectedTrues = savedTrues sd
            }

saveProgress :: FilePath -> GameState -> IO ()
saveProgress path = writeFile path . show . toSave

loadSave :: FilePath -> IO (Maybe SaveData)
loadSave path = do
    result <- try (readFile path >>= \s -> evaluate (length s) >> pure s)
    pure $ case result of
        Left (_ :: IOException) -> Nothing
        Right contents -> readMaybe contents

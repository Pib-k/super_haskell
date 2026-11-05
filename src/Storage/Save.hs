{-# LANGUAGE ScopedTypeVariables #-}

module Storage.Save where

import Control.Exception (IOException, try)
import Core.Types (PauseOption (Resume))
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Game.State

saveFile :: FilePath
saveFile = "save.json"

-- | Saves the game progress to a file
saveProgress :: FilePath -> GameState -> IO ()
saveProgress path gs = BL.writeFile path . encode $ newGameState
  where
    newGameState = gs{appMode = Paused Resume}

-- | Loads the game progress from a file
loadSave :: FilePath -> IO (Maybe GameState)
loadSave path = do
    result <- try (BL.readFile path)
    pure $ case result of
        Left (_ :: IOException) -> Nothing
        Right bytes -> decode bytes

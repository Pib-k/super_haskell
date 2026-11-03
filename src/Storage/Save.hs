{-# LANGUAGE ScopedTypeVariables #-}

module Storage.Save where

import Control.Exception (IOException, try)
import Core.Types (PauseOption (Resume))
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Game.State

saveFile :: FilePath
saveFile = "save.json"

saveProgress :: FilePath -> GameState -> IO ()
saveProgress path gs = BL.writeFile path . encode $ newGameState
  where
    newGameState = gs{appMode = Paused Resume}

loadSave :: FilePath -> IO (Maybe GameState)
loadSave path = do
    result <- try (BL.readFile path)
    pure $ case result of
        Left (_ :: IOException) -> Nothing
        Right bytes -> decode bytes

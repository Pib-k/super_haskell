module Main (main) where

import Graphics.Gloss.Interface.IO.Game
import IO
import Engine

main :: IO ()
main = do
    state0 <- initialState
    playIO
        (InWindow "Super Haskell" (800, 600) (100, 100))
        black
        60
        state0
        viewWorld
        handleInput
        stepWorld

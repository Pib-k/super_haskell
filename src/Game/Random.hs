{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Random where

import Data.Aeson
import System.Random.Internal (StdGen (..))
import System.Random.SplitMix (seedSMGen, unseedSMGen)

-- | JSON serialization instances for the RNG state, so level seeds can be properly saved and restored.
instance ToJSON StdGen where
    toJSON (StdGen gen) = toJSON (unseedSMGen gen)

instance FromJSON StdGen where
    parseJSON v = do
        (s, g) <- parseJSON v
        pure (StdGen (seedSMGen s g))

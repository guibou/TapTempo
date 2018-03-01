{-# LANGUAGE DataKinds, TypeApplications, TemplateHaskell #-}
module Model where

import GHC.TypeLits
import Data.Proxy
import Refined

-- | Runtime configuration of Tap Tempo
data Config = Config
  { precision :: RefinedPrecision -- ^ number of digits used to display the bmp
  , resetTime :: RefinedResetTime -- ^ time, in second before discarding samples
  , sampleSize :: RefinedSampleSize -- ^ size of the sliding window of samples
  }
  deriving (Show)

type MaxPrecision = 5
type RefinedPrecision = Refined (FromTo 0 MaxPrecision) Int
type RefinedResetTime = Refined Positive Int
type RefinedSampleSize = Refined Positive Int

-- runtime value
defaultResetTime :: RefinedResetTime
defaultResetTime = $$(refineTH 5)

defaultSampleSize :: RefinedSampleSize
defaultSampleSize = $$(refineTH 5)

defaultPrecision :: RefinedPrecision
defaultPrecision = $$(refineTH 0)

maxPrecision :: Int
maxPrecision = fromInteger (natVal (Proxy :: Proxy MaxPrecision))

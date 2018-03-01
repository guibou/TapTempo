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

-- type level value
type DefaultPrecision = 0
type DefaultSampleSize = 5
type DefaultResetTime = 5
type MaxPrecision = 5

type RefinedPrecision = Refined (FromTo 0 MaxPrecision) Int
type RefinedResetTime = Refined Positive Int
type RefinedSampleSize = Refined Positive Int

-- runtime value, this is ugly
defaultResetTime :: RefinedResetTime
defaultResetTime = $$(refineTH (fromInteger $ natVal (Proxy :: Proxy DefaultResetTime)))

defaultSampleSize :: RefinedSampleSize
defaultSampleSize = $$(refineTH (fromInteger $ natVal (Proxy :: Proxy DefaultSampleSize)))

defaultPrecision :: RefinedPrecision
defaultPrecision = $$(refineTH (fromInteger (natVal (Proxy :: Proxy DefaultPrecision))))

maxPrecision :: Int
maxPrecision = fromInteger (natVal (Proxy :: Proxy MaxPrecision))

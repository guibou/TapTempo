{-# LANGUAGE DataKinds, TypeApplications, TemplateHaskell #-}
module Default where

import GHC.TypeLits
import Data.Proxy
import Refined

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

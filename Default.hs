{-# LANGUAGE DataKinds, TypeApplications #-}
module Default where

import GHC.TypeLits
import Data.Proxy

-- type level value
type DefaultPrecision = 0
type DefaultSampleSize = 5
type DefaultResetTime = 5
type MaxPrecision = 5

-- runtime value, this is ugly
defaultResetTime = natVal (Proxy :: Proxy DefaultResetTime)
defaultSampleSize = natVal (Proxy :: Proxy DefaultSampleSize)
defaultPrecision = natVal (Proxy :: Proxy DefaultPrecision)
maxPrecision = natVal (Proxy :: Proxy MaxPrecision)

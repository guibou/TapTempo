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
defaultResetTime, defaultSampleSize, defaultPrecision, maxPrecision :: Int
defaultResetTime = fromInteger $ natVal (Proxy :: Proxy DefaultResetTime)
defaultSampleSize = fromInteger $ natVal (Proxy :: Proxy DefaultSampleSize)
defaultPrecision = fromInteger $ natVal (Proxy :: Proxy DefaultPrecision)
maxPrecision = fromInteger $ natVal (Proxy :: Proxy MaxPrecision)

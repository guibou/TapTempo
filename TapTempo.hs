{-# LANGUAGE DataKinds, TemplateHaskell, OverloadedStrings #-}
module TapTempo (
  Config(..),
  tapTempo
  ) where

import Refined
import System.Clock
import qualified System.IO as IO

import Default
import I18N

import Prelude hiding (putStrLn, putStr)
import Data.Text.IO (putStrLn, putStr)

-- | Runtime configuration of Tap Tempo
data Config = Config
  { precision :: Refined (FromTo 0 MaxPrecision) Int -- ^ number of digits used to display the bmp
  , resetTime :: Refined Positive Int -- ^ time, in second before discarding samples
  , sampleSize :: Refined Positive Int -- ^ size of the sliding window of samples
  }
  deriving (Show)

getRetOrQuit = do
  c <- getChar
  if c == '\n' || c == 'q'
    then pure c
    else getRetOrQuit

tapTempo :: Config -> IO ()
tapTempo config = do
  putStrLn (message MsgHello)
  IO.hSetBuffering IO.stdin IO.NoBuffering

  go []

  where
    go :: [Integer] -> IO ()
    go samples = do
      c <- getRetOrQuit

      case c of
        'q' -> putStrLn (message (MsgGoodBye))
        '\n' -> do
          t <- toNanoSecs <$> getTime Monotonic

          let t' = t:if resetTimeTooOld (resetTime config) t samples
                     then []
                     else samples

          if length t' <= 1
            then putStrLn (message (MsgHitMore))
            else do
              let bpm = computeBPM t'
              putStrLn (message (MsgTempo bpm (unrefine $ precision config)))
          go (take (unrefine (sampleSize config)) t')

computeBPM :: [Integer] -> Float
computeBPM l = bpm
  where
    elapsedTime = head l - last l
    meanTime = elapsedTime `div` (fromIntegral (length l))
    bpm = 60 / (fromInteger (meanTime) / (10 ^ 9))

resetTimeTooOld :: Refined Positive Int -> Integer -> [Integer] -> Bool
resetTimeTooOld _ _ [] = False
resetTimeTooOld bound t0 (t1:_) = abs (t0 - t1) > (fromIntegral $ unrefine bound) * 10 ^ 9

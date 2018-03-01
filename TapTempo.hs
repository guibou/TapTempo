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

-- | Runtime configuration of Tap Tempo
data Config = Config
  { precision :: Refined (FromTo 0 MaxPrecision) Int -- ^ number of digits used to display the bmp
  , resetTime :: Refined Positive Int -- ^ time, in second before discarding samples
  , sampleSize :: Refined Positive Int -- ^ size of the sliding window of samples
  }
  deriving (Show)

-- | The main loop
tapTempo :: Config -> IO ()
tapTempo config = do
  putStrLn (message MsgHello)
  IO.hSetBuffering IO.stdin IO.NoBuffering

  go []
  -- in case of any exception, the buffering stays to NoBuffering. I
  -- don't like that.

  putStrLn (message (MsgGoodBye))

  where
    go :: [TimeSpec] -> IO ()
    go samples = onReturnPressed $ do
      t <- getTime Monotonic

      let samples' = clipNumberOfSamples (sampleSize config) (clipOldSamples (resetTime config) (t:samples))

      case computeBPM samples' of
        Nothing -> putStrLn (message (MsgHitMore))
        Just bpm -> putStrLn (message (MsgTempo bpm (unrefine $ precision config)))
      go samples'

-- | execute the action if `<Return>` is pressed
--   returns if `q` is pressed
onReturnPressed :: IO () -> IO ()
onReturnPressed action = do
  c <- getChar
  case c of
    '\n' -> action
    'q' -> pure ()
    _ -> onReturnPressed action


-- | If possible, returns the beat per minutes of this sample sequence
computeBPM :: [TimeSpec] -> Maybe Float
computeBPM l
  | length l < 2 = Nothing
  | otherwise = Just bpm
  where
    elapsedTime = toNanoSecs (diffTimeSpec (head l) (last l))
    meanTime = elapsedTime `div` (fromIntegral (length l))
    bpm = 60 / (fromInteger (meanTime) / (10 ^ 9))

-- | Drop samples which are too old
clipOldSamples :: Refined Positive Int -> [TimeSpec] -> [TimeSpec]
clipOldSamples limit (x:y:_)
  | tooOld limit x y = [x]
clipOldSamples _ l = l

-- | Limit the sliding window size
clipNumberOfSamples :: Refined Positive Int -> [TimeSpec] -> [TimeSpec]
clipNumberOfSamples limit = take (unrefine limit)

tooOld :: Refined Positive Int -> TimeSpec -> TimeSpec -> Bool
tooOld limit a b = sec (diffTimeSpec a b) > (fromIntegral (unrefine limit))

{-# LANGUAGE DataKinds, TemplateHaskell, OverloadedStrings #-}
module TapTempo (
  Config(..),
  tapTempo
  ) where

import Refined
import System.Clock
import qualified System.IO as IO
import qualified Data.Sequence as Seq
import Data.Sequence hiding (length)

import Model
import I18N

-- | The main loop
tapTempo :: Config -> IO ()
tapTempo config = do
  putStrLn (message MsgHello)
  IO.hSetBuffering IO.stdin IO.NoBuffering

  go Empty
  -- in case of any exception, the buffering stays to NoBuffering. I
  -- don't like that.

  putStrLn (message (MsgGoodBye))

  where
    go :: Seq TimeSpec -> IO ()
    go samples = onReturnPressed $ do
      t <- getTime Monotonic

      let samples' = clipNumberOfSamples (sampleSize config) (clipOldSamples (resetTime config) (t :<| samples))

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
computeBPM :: Seq TimeSpec -> Maybe Float
computeBPM s@(first :<| (_ :|> last)) = Just bpm
  where
    elapsedTime = toNanoSecs (diffTimeSpec first last)
    meanTime = elapsedTime `div` (fromIntegral (Seq.length s))
    bpm = 60 / (fromInteger (meanTime) / (10 ^ (9 :: Integer)))
computeBPM _ = Nothing

-- | Drop samples which are too old
clipOldSamples :: Refined Positive Int -> Seq TimeSpec -> Seq TimeSpec
clipOldSamples limit (x :<| y :<| _)
  | tooOld limit x y = Seq.singleton x
clipOldSamples _ l = l

-- | Limit the sliding window size
clipNumberOfSamples :: Refined Positive Int -> Seq TimeSpec -> Seq TimeSpec
clipNumberOfSamples limit = Seq.take (unrefine limit)

tooOld :: Refined Positive Int -> TimeSpec -> TimeSpec -> Bool
tooOld limit a b = sec (diffTimeSpec a b) > (fromIntegral (unrefine limit))

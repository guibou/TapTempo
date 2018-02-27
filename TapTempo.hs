{-# LANGUAGE DataKinds, TemplateHaskell, OverloadedStrings #-}
import Options.Applicative
import Data.Semigroup ((<>))
import Text.Read (readEither)
import Refined
import System.Clock
import System.IO
import Formatting

import Version (versionString)
import Default

-- | Runtime configuration of Tap Tempo
data Config = Config
  { precision :: Refined (FromTo 0 MaxPrecision) Int
  , resetTime :: Refined Positive Int
  , sampleSize :: Refined Positive Int
  }
  deriving (Show)

sample :: Parser Config
sample = Config
      <$> option (eitherReader (\x -> refine =<< readEither x))
          ( long "precision"
         <> short 'p'
         <> help "changer le nombre de décimale du tempo à afficher."
         <> showDefaultWith (\x -> show (unrefine x))
         <> value (($$(refineTH defaultPrecision)) :: Refined (FromTo 0 MaxPrecision) Int)
         <> metavar ("(0.." ++ show maxPrecision ++ ")")
          )
      <*> option (eitherReader (\x -> refine =<< readEither x))
          ( long "reset-time"
         <> short 'r'
         <> help "changer le temps en seconde de remise à zéro du calcul."
         <> showDefaultWith (\x -> show (unrefine x))
         <> value (($$(refineTH defaultResetTime)) :: Refined Positive Int)
         <> metavar "(INT>0)" )
      <*> option (eitherReader (\x -> refine =<< readEither x))
          ( long "sample-size"
         <> short 's'
         <> help "changer le nombre d'échantillons nécessaires au calcul du tempo."
         <> showDefaultWith (\x -> show (unrefine x))
         <> value (($$(refineTH defaultSampleSize)) :: Refined Positive Int)
         <> metavar "(INT>0)" )


main :: IO ()
main = do
  p <- execParser opts
  tapTempo p
  where
    opts = info (sample <**> helper <**> version)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

version = infoOption versionBanner ( long "version"
                                     <> short 'v'
                                     <> help "Show the version number")

versionBanner = unlines [
  "Tap Tempo " ++ $(versionString)
  ,"Copyright (C) 2018 Guillaume Bouchard"
  ,"License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>"
  ,"This is free software: you are free to change and redistribute it."
  ,"There is NO WARRANTY, to the extent permitted by law."
  ]

getRetOrQuit = do
  c <- getChar
  if c == '\n' || c == 'q'
    then pure c
    else getRetOrQuit

tapTempo :: Config -> IO ()
tapTempo config = do
  putStrLn "Appuyer sur la touche entrée en cadence (q pour quitter)."
  hSetBuffering stdin NoBuffering

  go []

  where
    go :: [Integer] -> IO ()
    go samples = do
      c <- getRetOrQuit

      case c of
        'q' -> putStrLn "Bye Bye"
        '\n' -> do
          t <- toNanoSecs <$> getTime Monotonic

          let t' = t:if resetTimeTooOld (resetTime config) t samples
                     then []
                     else samples

          putChar '\r'
          if length t' <= 1
            then putStr "[Hit enter key one more time to start bpm computation...]"
            else do
              let bpm = computeBPM t'
              putStr (formatToString ("Tempo:" % fixed (unrefine $ precision config) % " bpm\t") bpm)
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

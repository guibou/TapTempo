{-# LANGUAGE DataKinds, TemplateHaskell, OverloadedStrings #-}
import Options.Applicative
import Data.Semigroup ((<>))
import Text.Read (readEither)
import Refined
import System.Clock
import qualified System.IO as IO

import Version (versionString)
import Default
import I18N

import Prelude hiding (putStrLn, putStr)
import Data.Text.IO (putStrLn, putStr)

-- TODO:
{-
   Localisation:
         - les champs par défaut de optparse-applicative ("Usage", "Available options")
         - les "default" de optparse-applicative
         - les messages d'erreurs de optparse-applicative
         - les messages d'erreurs de refined
   Localisation:
         - Detecter la locale système
-}

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
         <> help (messageS MsgCLIHelpPrecision)
         <> showDefaultWith (\x -> show (unrefine x))
         <> value (($$(refineTH defaultPrecision)) :: Refined (FromTo 0 MaxPrecision) Int)
         <> metavar ("(0.." ++ show maxPrecision ++ ")")
          )
      <*> option (eitherReader (\x -> refine =<< readEither x))
          ( long "reset-time"
         <> short 'r'
         <> help (messageS MsgCLIHelpResetTime)
         <> showDefaultWith (\x -> show (unrefine x))
         <> value (($$(refineTH defaultResetTime)) :: Refined Positive Int)
         <> metavar "(INT>0)" )
      <*> option (eitherReader (\x -> refine =<< readEither x))
          ( long "sample-size"
         <> short 's'
         <> help (messageS MsgCLIHelpSampleSize)
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
     <> progDesc (messageS MsgCLIDescription)
     <> header (messageS MsgCLIHeader)
      )

    helper = abortOption ShowHelpText (  long "help"
                                    <> short 'h'
                                    <> help (messageS MsgCLIHelp)
                                    )

    version :: Parser (a -> a)
    version = infoOption versionBanner ( long "version"
                                        <> short 'v'
                                        <> help (messageS MsgCLIVersion))

versionBanner :: String
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

          putStrLn ""
          if length t' <= 1
            then putStrLn (message (MsgHitMore))
            else do
              let bpm = computeBPM t'
              putStr (message (MsgTempo bpm (unrefine $ precision config)))
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

{-# LANGUAGE DataKinds, TemplateHaskell #-}
import Options.Applicative
import Data.Semigroup ((<>))
import Text.Read (readEither)
import Refined

import Version (versionString)
import Default

-- | Runtime configuration of Tap Tempo
data Config = Config
  { precision :: Refined (FromTo 0 MaxPrecision) Integer
  , resetTime :: Refined NonNegative Integer
  , sampleSize :: Refined NonNegative Integer
  }
  deriving (Show)

sample :: Parser Config
sample = Config
      <$> option (eitherReader (\x -> refine =<< readEither x))
          ( long "precision"
         <> short 'p'
         <> help "changer le nombre de décimale du tempo à afficher."
         <> showDefaultWith (\x -> show (unrefine x))
         <> value (($$(refineTH defaultPrecision)) :: Refined (FromTo 0 MaxPrecision) Integer)
         <> metavar ("(0.." ++ show maxPrecision ++ ")")
          )
      <*> option (eitherReader (\x -> refine =<< readEither x))
          ( long "reset-time"
         <> short 'r'
         <> help "changer le temps en seconde de remise à zéro du calcul."
         <> showDefaultWith (\x -> show (unrefine x))
         <> value (($$(refineTH defaultResetTime)) :: Refined NonNegative Integer)
         <> metavar "(FLOAT>=0)" )
      <*> option (eitherReader (\x -> refine =<< readEither x))
          ( long "sample-size"
         <> short 's'
         <> help "changer le nombre d'échantillons nécessaires au calcul du tempo."
         <> showDefaultWith (\x -> show (unrefine x))
         <> value (($$(refineTH defaultSampleSize)) :: Refined NonNegative Integer)
         <> metavar "(INT>=0)" )


main :: IO ()
main = do
  p <- execParser opts
  print p
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

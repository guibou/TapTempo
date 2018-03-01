{-# LANGUAGE DataKinds, TemplateHaskell, OverloadedStrings #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Read (readEither)
import Refined

import Version (versionString)
import Default
import I18N

import TapTempo

options :: Parser Config
options = Config
      <$> option (eitherReader (\x -> refine =<< readEither x))
          ( long "precision"
         <> short 'p'
         <> help (message MsgCLIHelpPrecision)
         <> showDefaultWith (\x -> show (unrefine x))
         <> value defaultPrecision
         <> metavar ("(0.." ++ show maxPrecision ++ ")")
          )
      <*> option (eitherReader (\x -> refine =<< readEither x))
          ( long "reset-time"
         <> short 'r'
         <> help (message MsgCLIHelpResetTime)
         <> showDefaultWith (\x -> show (unrefine x))
         <> value defaultResetTime
         <> metavar "(INT>0)" )
      <*> option (eitherReader (\x -> refine =<< readEither x))
          ( long "sample-size"
         <> short 's'
         <> help (message MsgCLIHelpSampleSize)
         <> showDefaultWith (\x -> show (unrefine x))
         <> value defaultSampleSize
         <> metavar "(INT>0)" )

versionBanner :: String
versionBanner = unlines [
  "Tap Tempo " ++ $(versionString)
  ,"Copyright (C) 2018 Guillaume Bouchard"
  ,"License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>"
  ,"This is free software: you are free to change and redistribute it."
  ,"There is NO WARRANTY, to the extent permitted by law."
  ]

-- | Just parse command line options and run tapTempo
main :: IO ()
main = execParser opts >>= tapTempo
  where
    opts = info (options <**> helpOption <**> version)
      ( fullDesc
        <> progDesc (message MsgCLIDescription)
        <> header (message MsgCLIHeader))

    helpOption = abortOption ShowHelpText ( long "help"
                                            <> short 'h'
                                            <> help (message MsgCLIHelp))

    version = infoOption versionBanner ( long "version"
                                         <> short 'v'
                                         <> help (message MsgCLIVersion))

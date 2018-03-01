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
        <> progDesc (messageS MsgCLIDescription)
        <> header (messageS MsgCLIHeader))

    helpOption = abortOption ShowHelpText ( long "help"
                                            <> short 'h'
                                            <> help (messageS MsgCLIHelp))

    version = infoOption versionBanner ( long "version"
                                         <> short 'v'
                                         <> help (messageS MsgCLIVersion))

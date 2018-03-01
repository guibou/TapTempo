{-# LANGUAGE TemplateHaskell #-}
module Version where

import Language.Haskell.TH (runIO, Q, Exp)
import System.Process (readProcess)

-- | Extract the version string at compile time using git describe
versionString :: Q Exp
versionString = do
  s <- runIO (readProcess "git" ["describe"] "")
  let striped = init s
  [| striped |]

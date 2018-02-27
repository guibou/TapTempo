{-# LANGUAGE TemplateHaskell #-}
module Version where

import Language.Haskell.TH (runQ, runIO, Q)
import System.Process (readProcess)

-- The version string is extrated at compile time using git describe
versionString = do
  s <- runIO (readProcess "git" ["describe"] "")
  let striped = init s
  [| striped |]

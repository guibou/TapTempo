{-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses, FlexibleContexts #-}
module I18N
  (message, messageS, TapTempoMessage(..))
where

import Text.Shakespeare.I18N
import qualified Formatting
import qualified Data.Text as Text
import System.Environment (lookupEnv)
import System.IO.Unsafe

data TapTempo = TapTempo

mkMessage "TapTempo" "messages/" ("en")

message m = renderMessage TapTempo [unsafeCurrentLocale] m
messageS m = Text.unpack (message m)

showBpm :: Int -> Float -> String
showBpm precision bpm = Formatting.formatToString (Formatting.fixed precision) bpm

-- | The local currently in use by the system
--   This value is unsafe if your system local suddently changes
unsafeCurrentLocale :: Text.Text
unsafeCurrentLocale = unsafePerformIO getCurrentLocal

-- | Read the current local
getCurrentLocal :: IO Text.Text
getCurrentLocal = do
  -- I don't know what I'm doing...
  locale <- lookupEnv "LANG"
  case locale of
    Just l -> pure (Text.pack (take 2 l))
    Nothing -> pure "en"

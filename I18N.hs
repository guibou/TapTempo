{-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses, FlexibleContexts #-}
module I18N
  (message, messageS, TapTempoMessage(..))
where

import Text.Shakespeare.I18N
import qualified Formatting
import qualified Data.Text as Text

data TapTempo = TapTempo

mkMessage "TapTempo" "messages/" ("en")

message m = renderMessage TapTempo [("fr")] m
messageS m = Text.unpack $ renderMessage TapTempo [("fr")] m

showBpm :: Int -> Float -> String
showBpm precision bpm = Formatting.formatToString (Formatting.fixed precision) bpm

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Static where

import qualified Data.Text as Text
import Data.FileEmbed

css :: Text.Text
css = $(embedStringFile "static/styles.css")

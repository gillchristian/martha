{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Static
  ( css,
    highlightJS,
    highlightCSS,
    bulmaCSS,
  )
where

import Data.FileEmbed
import qualified Data.Text as Text

css :: Text.Text
css = $(embedStringFile "static/styles.css")

highlightJS :: Text.Text
highlightJS = $(embedStringFile "static/highlight.pack.js")

highlightCSS :: Text.Text
highlightCSS = $(embedStringFile "static/highlight.github.css")

bulmaCSS :: Text.Text
bulmaCSS = $(embedStringFile "static/bulma.css")

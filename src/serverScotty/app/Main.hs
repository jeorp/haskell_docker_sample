{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mconcat)

main = S.scotty 3000 $
  S.get "/:word" $ do
    beam <- S.param "word"
    S.html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

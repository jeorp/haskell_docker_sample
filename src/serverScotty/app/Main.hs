{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mconcat)

main = S.scotty 3000 $
  S.get "/:word" $ do
    S.html $ renderHtml render

-- html template by Blaze.Html5
render = do
  H.html $ do
    H.body $ do
      H.h1 "My todo list"
      H.ul $ do
        H.li "learn haskell"
        H.li "make a website"
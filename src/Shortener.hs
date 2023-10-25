{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

shortener :: IO ()
shortener =
  scotty 3000 $
    get "/" $
      html $ renderHtml $
        H.html $
          H.body $ do
            H.h1 "Shortener (using blaze-html)"
            H.form H.! A.method "post" H.! A.action "/" $ do
              H.input H.! A.type_ "text" H.! A.name "url"
              H.input H.! A.type_ "submit"

{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Web.Scotty

shortener :: IO ()
shortener =
  scotty 3000 $
    get "/" $
      html "<h1>Shortener</h1>"

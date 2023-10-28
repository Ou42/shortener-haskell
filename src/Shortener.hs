{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
-- import qualified Data.Text.Lazy.Encoding as T
-- import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy (fromStrict)
import qualified Network.HTTP.Types.Status as Status
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

shortener :: IO ()
shortener = do
  urlsR <- newIORef (1 :: Int, mempty :: Map Int Text)
  scotty 3000 $ do
    get "/" $ do
      (_, urls) <- liftIO $ readIORef urlsR
      html $ renderHtml $
        H.html $
          H.body $ do
            H.h1 "Shortener (using blaze-html)"
            H.form H.! A.method "post" H.! A.action "/" $ do
              H.input H.! A.type_ "text" H.! A.name "url"
              H.input H.! A.type_ "submit"
            H.table $
              for_ (M.toList urls) $ \(i, url) ->
                H.tr $ do
                  H.td (H.toHtml i)
                  H.td (H.text url)
    post "/" $ do
      url <- param "url"
      liftIO $ modifyIORef urlsR $
        \(i, urls) ->
          (i + 1, M.insert i url urls)
      redirect "/"
    get "/:id" $ do
      urlID <- param "id"
      -- if `param "url"` is used, get error, param not found
      (_, urls) <- liftIO $ readIORef urlsR

      -- the following works, outputs to the terminal:
      -- liftIO $ putStrLn $ "id = " <> urlID -- OUTPUTS TO THE TERMINAL

      -- the following sorta works ...
      -- redirect $ fromStrict $ fromJust $ M.lookup urlID urls

      {-
      let maybeDestURL = M.lookup urlID urls
      case maybeDestURL of
        Nothing -> status Status.status404
        Just destURL -> redirect destURL

        ERROR:
        • Couldn't match expected type ‘Data.Text.Internal.Lazy.Text’
              with actual type ‘Text’
          NB: ‘Data.Text.Internal.Lazy.Text’
                is defined in ‘Data.Text.Internal.Lazy’
              ‘Text’ is defined in ‘Data.Text.Internal’
      -}

      -- the following outputs to the terminal
      let maybeDestURL = M.lookup urlID urls
      case maybeDestURL of
        Nothing -> status Status.status404
        -- Just destURL -> liftIO $ putStrLn $ unpack destURL
        -- Just destURL -> liftIO $ putStrLn $ show destURL -- linter: why not use print ?!
        -- Just destURL -> liftIO $ print destURL
        Just destURL -> liftIO $ print (urlID, destURL)

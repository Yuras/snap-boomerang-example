{-# LANGUAGE OverloadedStrings #-}

module Site
(
  app
)
where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.Trans
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist (render, heistInit, heistLocal)
import Snap.Util.FileServe (serveDirectory)
import Heist.Interpreted (bindString)

import Application
import Sitemap

routeSitemap :: Sitemap -> AppHandler ()
routeSitemap Home = render "index"
routeSitemap NewPost = heistLocal (bindString "post" $ showUrl $ Post "Very Interesting Post") $ render "new_post"
routeSitemap (Post "") = redirectSM Home
routeSitemap (Post pid) = writeText ("Post: " `T.append` pid)

notFound :: MonadSnap m => m ()
notFound = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  writeText "Not Found"

routes :: [(ByteString, Handler App App ())]
routes = [
  ("", mkRoute routeSitemap),
  ("", serveDirectory "static"),
  ("", notFound)
  ]

redirectSM :: MonadSnap m => Sitemap -> m ()
redirectSM = redirect . T.encodeUtf8 . showUrl

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    liftIO $ print $ heistUrl NewPost
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    addRoutes routes
    return $ App h

{-# LANGUAGE OverloadedStrings #-}

module Site
(
  app
)
where

import Data.ByteString (ByteString)
import Data.Monoid (mempty)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist (heistInit,addConfig)
import Heist (hcInterpretedSplices)
import Heist.SpliceAPI ((##),Splices)
import Heist.Interpreted (Splice,textSplice)
import Snap.Util.FileServe (serveDirectory)

import Application
import Sitemap

notFound :: MonadSnap m => m ()
notFound = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  writeText "Not Found"

routes :: [(ByteString, Handler App App ())]
routes = [
  ("", mkRoute),
  ("", serveDirectory "static"),
  ("", notFound)
  ]

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    addRoutes routes
    addConfig h $ mempty { hcInterpretedSplices = routeSplices }  
    return $ App h

routeSplices :: Splices (Splice (Handler App App))
routeSplices = do
  "HomeR" ## textSplice $ showUrl Home
  "NewPostR" ## textSplice $ showUrl NewPost

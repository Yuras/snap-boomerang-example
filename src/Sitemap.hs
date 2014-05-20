{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Sitemap
where

import Prelude hiding (id,(.))

import Control.Applicative ((<$>))
import Control.Monad.Trans  (lift)
import Data.Monoid (mconcat)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Category (id,(.))
import Snap.Core
import Text.Boomerang.TH (makeBoomerangs)
import Text.XmlHtml hiding (render)
import Snap.Snaplet.Heist (render, heistLocal)
import Heist.Interpreted (bindString)
import Heist.Compiled (textSplice,pureSplice)

import Application
import Web.Routes           (Site(..), RouteT(..), decodePathInfo, encodePathInfo, runSite)
import Web.Routes.Boomerang ((</>),anyText,boomerangSiteRouteT)

data Sitemap 
  = Home 
  | NewPost 
  |  Post Text
  deriving (Eq,Show)
makeBoomerangs ''Sitemap

sitemap = mconcat 
  [ rHome 
  , "post" </> mconcat 
    [ "new" . rNewPost 
    , "show" . rPost </> anyText
    ]
  ]

handle :: Sitemap -> RouteT Sitemap AppHandler ()
handle url = lift $ 
  case url of
    Home     -> render "index"
    NewPost  -> heistLocal (bindString "post" $ showUrl $ Post "Very Interesting Post") $ render "new_post"
    Post ""  -> redirectSM Home
    Post pid -> writeText ("Post: " `T.append` pid)

redirectSM :: MonadSnap m => Sitemap -> m ()
redirectSM = redirect . T.encodeUtf8 . showUrl

site :: Site Sitemap (AppHandler ())
site = boomerangSiteRouteT handle sitemap

showUrl :: Sitemap -> Text
showUrl url =
  let (ps, params) = formatPathSegments site url
  in (encodePathInfo ps params)

mkRoute :: AppHandler ()
mkRoute = do
  pps <- (decodePathInfo . B.dropWhile (== '/') . rqPathInfo) <$> getRequest
  either (const pass) id $ runSite "" site pps 


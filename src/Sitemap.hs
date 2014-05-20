{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Sitemap
where

import Prelude hiding (id,(.))

import Control.Monad.Trans  (MonadIO(liftIO))
import Data.Monoid (mconcat)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Control.Category (id,(.))
import Snap.Core
import Web.Routes (decodePathInfo, encodePathInfo,formatPathSegments,parsePathSegments)
import Web.Routes.TH (derivePathInfo)
import Web.Routes.Boomerang (Router, (<>), (</>), anyText)
import Text.Boomerang.TH (makeBoomerangs)
import Text.XmlHtml hiding (render)

import Web.Routes           (Site(..), RouteT(..), decodePathInfo, encodePathInfo, runSite, showURL)
import Web.Routes.Boomerang (Router, (<>), (</>), int, parse1, boomerangSiteRouteT)

data Sitemap 
  = Home 
  | NewPost 
  |  Post Text
  deriving (Eq,Show)
makeBoomerangs ''Sitemap
derivePathInfo ''Sitemap

sitemap = mconcat 
  [ rHome 
  , "post" </> mconcat 
    [ "new" . rNewPost 
    , "show" . rPost </> anyText
    ]
  ]

handle :: Sitemap -> RouteT Sitemap IO ()
handle url =
   case url of
     _ -> do liftIO $ print url
             s <- showURL url
             liftIO . putStrLn . T.unpack $ s

site :: Site Sitemap (IO ())
site = boomerangSiteRouteT handle sitemap

showUrl :: Sitemap -> Text
showUrl url =
  let (ps, params) = formatPathSegments site url
  in (encodePathInfo ps params)

heistUrl :: Sitemap -> [Node]
heistUrl = return . TextNode . showUrl

mkRoute :: MonadSnap m => (Sitemap -> m ()) -> m ()
mkRoute = undefined



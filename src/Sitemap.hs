{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sitemap
where

import Prelude hiding ((.))
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Control.Category ((.))
import Snap.Core
import Web.Routes (decodePathInfo, encodePathInfo)
import Web.Routes.TH (derivePathInfo)
import Web.Routes.Boomerang (Router, (<>), (</>), anyString, parseStrings, unparseStrings)
import Text.Boomerang.TH (derivePrinterParsers)
import Text.XmlHtml hiding (render)

data Sitemap =
  Home |
  NewPost |
  Post String
$(derivePathInfo ''Sitemap)
$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap
sitemap =
  (
  rHome <>
  "post" </> (
    "new" . rNewPost <>
    "show" . rPost </> anyString
    )
  )

showUrl :: Sitemap -> Text
showUrl = addSlash . flip encodePathInfo [] . map T.pack . fromJust . unparseStrings sitemap
  where
  addSlash txt | T.null txt = "/" `T.append` txt
  addSlash txt = txt

heistUrl :: Sitemap -> [Node]
heistUrl = return . TextNode . showUrl

mkRoute :: MonadSnap m => (Sitemap -> m ()) -> m ()
mkRoute router = do
  rq <- getRequest
  case parseStrings sitemap $ map T.unpack $ decodePathInfo $ dropSlash $ rqPathInfo rq of
    Left _ -> pass
    Right url -> router url
  where
  dropSlash s = if ((B.pack "/") `B.isPrefixOf` s) 
                  then B.tail s
                  else s

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Transloadit where

import           Control.Applicative
import           Control.Monad
import           Crypto.Hash
import           Data.Aeson
import           Data.Aeson.Encode       (encodeToTextBuilder)
import           Data.Aeson.Types
import           Data.Byteable
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import           Data.Monoid
import           Data.Text
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Builder  (fromText, toLazyText)
import           Data.Text.Lazy.Encoding
import           Data.Time
import           System.Locale
import           Text.Julius
import           Yesod                   hiding (Key)
import           Yesod.Core
import           Yesod.Form.Jquery       (YesodJquery (..))

class YesodTransloadit master where
  transloaditRoot :: master -> Text
  transloaditRoot _ = "https://assets.transloadit.com/js/"

newtype Secret = Secret { secret :: BS.ByteString } deriving (Eq, Show)
newtype Key = Key { key :: Text } deriving (Eq, Show)
newtype Template = Template { template :: Text } deriving (Eq, Show)

type FormIdent = Text

data TransloaditParams = TransloaditParams {
  authExpires         :: UTCTime,
  transloaditKey      :: Key,
  transloaditTemplate :: Template,
  formIdent           :: Text
} deriving (Show)

data TransloaditResponse = TransloaditResponse { raw :: Text } deriving (Show)

data Upload = Upload Text deriving (Show)
instance FromJSON Upload where
  parseJSON (Object o) = Upload <$> (o .: "ssl_url")

data TransloaditResults = TransloaditResults {
  results :: [Upload]
} deriving (Show)

instance FromJSON TransloaditResults where
  parseJSON (Object o) = TransloaditResults <$> (o .: "uploads")
  parseJSON _ = mzero

formatExpiryTime :: UTCTime -> Text
formatExpiryTime = pack . formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S+00:00"

instance ToJSON TransloaditParams where
  toJSON (TransloaditParams a (Key k) (Template t) _) = object [
      "auth" .= object [
        "key" .= k,
        "expires" .= (formatExpiryTime a)
      ],
      "template_id" .= t
    ]

-- http://www.yesodweb.com/blog/2012/10/jqplot-widget
encodeText :: ToJSON a => a -> TL.Text
encodeText = toLazyText . encodeToTextBuilder . toJSON

type Signature = String

sign :: TransloaditParams -> BS.ByteString -> Signature
sign cfg s = (show . hmacGetDigest) h
  where h :: HMAC SHA1
        h = hmac s ((BSL.toStrict . encode) cfg)

transloadIt :: (YesodJquery m, YesodTransloadit m) => TransloaditParams -> Secret -> WidgetT m IO Signature
transloadIt t@(TransloaditParams {..}) (Secret s) = do
  master <- getYesod
  let root = transloaditRoot master
      signature = sign t s
  addScriptEither $ urlJqueryJs master
  addScriptRemote $ root <> "jquery.transloadit2-v2-latest.js"
  toWidget [julius|
     $(function() {
      $('##{rawJS formIdent}').transloadit({
        wait : true,
        params : JSON.parse('#{(rawJS . encodeText) t}')
      });
    });
  |]
  return signature

handleTransloadit = do
  d <- runInputPost $ TransloaditResponse <$> ireq hiddenField "transloadit"
  let r = ((decode . encodeUtf8 . TL.fromStrict) (raw d)) :: Maybe TransloaditResults
  return $ fmap results r

{- Example web service demonstrating usage of the transloadIt widget -}

data Test = Test
mkYesod "Test" [parseRoutes| / HomeR GET POST |]

instance Yesod Test
instance YesodJquery Test
instance YesodTransloadit Test

instance RenderMessage Test FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  now <- liftIO getCurrentTime

  -- Create an id for your form
  ident <- newIdent

  -- Create some Transloadit params, you need: Expiry time; Api key; Template Id; Form id
  let expiry = addUTCTime 3600 now
      key = Key "my_key"
      template = Template "a0db71d0d8af11e4b498edf9af585324"
      params = TransloaditParams expiry key template ident

  -- Load the widget, and retrieve the given signature
  sig <- transloadIt params (Secret "my_secret")

  -- Create a form
  [whamlet|
    <form id="#{ident}" action=@{HomeR} method="POST">
      <input type="hidden" name="signature" value="#{sig}">
      <input name="my_file" type="file">
      <input type="submit" value="Upload">
  |]
  return ()

postHomeR :: Handler Html
postHomeR = defaultLayout $ do
  results <- handleTransloadit
  [whamlet|
    $maybe uploads <- results
      $forall (Upload url) <- uploads
        <img src="#{url}"/>
    $nothing
      No results
  |]
  return ()

exampleServer = warp 3000 Test

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns          #-}

module Web.Transloadit where

import           Control.Applicative
import           Control.Monad
import           Crypto.Hash
import           Data.Aeson
import           Data.Aeson.Encode       (encodeToTextBuilder)
import           Data.Aeson.Lens hiding (key)
import           qualified Data.Aeson.Lens as AL
import           Data.Aeson.Types
import           Data.Byteable
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import           Control.Lens.Operators hiding ((.=))
import           Data.Maybe
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
  formIdent           :: Text,
  transloaditSecret   :: Secret
} deriving (Show)

data TransloaditResponse = TransloaditResponse { raw :: Text, token :: Text } deriving (Show)

data Upload = Upload Text deriving (Show)
instance FromJSON Upload where
  parseJSON (Object o) = Upload <$> (o .: "ssl_url")

formatExpiryTime :: UTCTime -> Text
formatExpiryTime = pack . formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S+00:00"

instance ToJSON TransloaditParams where
  toJSON (TransloaditParams a (Key k) (Template t) _ _) = object [
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

sign :: TransloaditParams -> Signature
sign cfg = (show . hmacGetDigest) h
  where h :: HMAC SHA1
        h = hmac (s cfg) ((BSL.toStrict . encode) cfg)
        s (transloaditSecret -> Secret s') = s'

transloadIt :: (YesodJquery m, YesodTransloadit m) => TransloaditParams -> WidgetT m IO Signature
transloadIt t@(TransloaditParams {..}) = do
  master <- getYesod
  let root = transloaditRoot master
      signature = sign t
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

tokenText :: (YesodJquery m, YesodTransloadit m) => WidgetT m IO Text
tokenText = do
  csrfToken <- fmap reqToken getRequest
  return $ fromMaybe mempty csrfToken

handleTransloadit :: (RenderMessage m FormMessage, YesodJquery m, YesodTransloadit m) => WidgetT m IO (Maybe Text)
handleTransloadit = do
  d <- runInputPost $ TransloaditResponse <$> ireq hiddenField "transloadit"
                                          <*> ireq hiddenField "_token"

  t <- tokenText

  return $ case (token d == t) of
    True -> return $ raw d
    _ -> Nothing

extractFirstResult :: AsValue s => Text -> Maybe s -> Maybe Value
extractFirstResult _ Nothing = Nothing
extractFirstResult k (Just uploads) = uploads ^? AL.key "results" . AL.key k . nth 0 . AL.key "ssl_url"

{-
-- Example web service demonstrating usage of the transloadIt widget

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
      template = Template "my_template"
      secret = Secret "my_secret"
      params = TransloaditParams expiry key template ident secret

  -- Load the widget, and retrieve the given signature
  sig <- transloadIt params

  -- CSRF considerations
  t <- tokenText

  -- Create a form
  [whamlet|
    <form id="#{ident}" action=@{HomeR} method="POST">
      <input type="hidden" name="_token" value="#{t}">
      <input type="hidden" name="signature" value="#{sig}">
      <input type="file" name="my_file">
      <input type="submit" value="Upload">
  |]
  return ()

postHomeR :: Handler Html
postHomeR = defaultLayout $ do
  results <- handleTransloadit

  -- my_template contains a step called "cropped_thumb"
  case extractFirstResult "cropped_thumb" results of
    Just (String url) -> [whamlet| <img src="#{url}"/> |]
    _ -> [whamlet| No results :( |]

  return ()

exampleServer = warp 4567 Test
-}

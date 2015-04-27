{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Yesod.Transloadit (
    YesodTransloadit(..),
    mkParams,
    transloadIt,
    handleTransloadit,
    tokenText,
    extractFirstResult,
    ParamsResult,
    ParamsError(..),
    Key(..),
    Template(..),
    Secret(..),
    TransloaditParams,
    Signature
  ) where

import           Control.Applicative
import           Control.Lens.Operators hiding ((.=))
import           Control.Monad          (mzero)
import           Crypto.Hash
import           Data.Aeson
import           Data.Aeson.Lens        hiding (key)
import qualified Data.Aeson.Lens        as AL
import qualified Data.ByteString        as BS
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import           Data.Time
import           System.Locale
import           Text.Julius
import           Yesod                  hiding (Key)
import           Yesod.Form.Jquery      (YesodJquery (..))

-- | Typeclass for your website to enable using Transloadit.
class YesodTransloadit master where
  -- | Override the 'transloaditRoot' to point at a different base Javascript directory.
  -- The default settings will load assets from assets.transloadit.com.
  transloaditRoot :: master -> Text
  transloaditRoot _ = "https://assets.transloadit.com/js/"
  {-# MINIMAL #-}

newtype Secret = Secret { secret :: BS.ByteString } deriving (Eq, Show)
newtype Key = Key { key :: Text } deriving (Eq, Show)
newtype Template = Template { template :: Text } deriving (Eq, Show)

data TransloaditParams = TransloaditParams {
  authExpires         :: UTCTime,
  transloaditKey      :: Key,
  transloaditTemplate :: Template,
  formIdent           :: Text,
  transloaditSecret   :: Secret
} deriving (Show)

data ParamsError = UnknownError
type ParamsResult = Either ParamsError TransloaditParams

-- | Smart constructor for Transloadit params
mkParams :: UTCTime      -- ^ When the Transloadit signature should expire
         -> Key          -- ^ Transloadit key
         -> Template     -- ^ The Template to use in Transloadit
         -> Text         -- ^ The id of the form to attach to
         -> Secret       -- ^ Transloadit Secret
         -> ParamsResult
mkParams u k t f s = return (TransloaditParams u k t f s)

data TransloaditResponse = TransloaditResponse { raw :: Text, token :: Text } deriving (Show)

data Upload = Upload Text deriving (Show)
instance FromJSON Upload where
  parseJSON (Object o) = Upload <$> (o .: "ssl_url")
  parseJSON _ = mzero

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

-- encodeParams is similar to the exported ToJSON instance, except that it gives us the same order
-- output of keys each time. This is very useful for testing that signatures are correct.
encodeParams :: TransloaditParams -> Text
encodeParams (TransloaditParams a (Key k) (Template t) _ _) = mconcat [
  "{\"auth\":{\"expires\":\"", (formatExpiryTime a),
  "\",\"key\":\"", k, "\"},",
  "\"template_id\":\"", t, "\"}"]

type Signature = Text

sign :: TransloaditParams -> Signature
sign cfg = (pack . show . hmacGetDigest) h
  where h :: HMAC SHA1
        h = hmac (s cfg) ((encodeUtf8 . encodeParams) cfg)
        s (transloaditSecret -> Secret s') = s'

-- | Calculate the signature, and embed Javascript to attach Transloadit to the form.
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
        params : JSON.parse('#{(rawJS . encodeParams) t}')
      });
    });
  |]
  return signature

-- | Helper method to grab the current CSRF token from the session. Returns 'mempty' if 'Nothing'
-- could be found.
tokenText :: (YesodJquery m, YesodTransloadit m) => WidgetT m IO Text
tokenText = do
  csrfToken <- fmap reqToken getRequest
  return $ fromMaybe mempty csrfToken

-- | Helper method to pull the Transloadit response and the CSRF token (named @_token@) from the request.
handleTransloadit :: (RenderMessage m FormMessage, YesodJquery m, YesodTransloadit m) => WidgetT m IO (Maybe Text)
handleTransloadit = do
  d <- runInputPost $ TransloaditResponse <$> ireq hiddenField "transloadit"
                                          <*> ireq hiddenField "_token"

  t <- tokenText

  return $ case (token d == t) of
    True -> return $ raw d
    _ -> Nothing

-- | Helper method to pull the first @ssl_url@ from the Transloadit response.
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

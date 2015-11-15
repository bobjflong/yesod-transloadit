{-# LANGUAGE CPP                   #-}
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
    nthStepResult,
    StepResult,
    resultId,
    name,
    baseName,
    extension,
    mime,
    field,
    url,
    sslUrl,
    ParamsResult,
    ParamsError(..),
    Key(..),
    Template(..),
    Secret(..),
    TransloaditParams,
    Signature,
    module Control.Lens,
    module Data.Time
  ) where

import           Codec.MIME.Parse
import           Codec.MIME.Type
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Crypto.Hash
import           Data.Aeson
import           Data.Aeson.Lens               hiding (key)
import qualified Data.Aeson.Lens               as AL
import qualified Data.ByteString               as BS
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import           Data.Time
import           Network.URI
import           Text.Julius
import           Yesod                         hiding (Key)
import           Yesod.Form.Jquery             (YesodJquery (..))
import           Yesod.Transloadit.OrderedJSON hiding (encode)
import qualified Yesod.Transloadit.OrderedJSON as OJ
#if MIN_VERSION_time(1,5,0)
#else
import           System.Locale                 (defaultTimeLocale)
#endif

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

-- | The result of the execution of a single step
data StepResult = StepResult {
  _resultId  :: Text,
  _name      :: Text,
  _baseName  :: Text,
  _extension :: Text,
  _mime      :: Maybe Type,
  _field     :: Text,
  _url       :: Maybe URI,
  _sslUrl    :: Maybe URI
} deriving (Show)

$(makeLenses ''StepResult)

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

formatExpiryTime :: UTCTime -> Text
formatExpiryTime = pack . formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S+00:00"

-- encodeParams is similar to the exported ToJSON instance, except that it gives us the same order
-- output of keys each time. This is very useful for testing that signatures are correct.
encodeParams :: TransloaditParams -> Text
encodeParams (TransloaditParams a (Key k) (Template t) _ _) = OJ.encode params
  where params = obj [
                   "auth" `is` obj [
                                 "expires" `is` str (formatExpiryTime a),
                                 "key" `is` str k
                               ],
                   "template_id" `is` str t
                 ]

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
      divId = mconcat ["#", formIdent]
  addScriptEither $ urlJqueryJs master
  addScriptRemote $ root <> "jquery.transloadit2-v2-latest.js"
  toWidget [julius|
     $(function() {
      $(#{toJSON divId}).transloadit({
        wait : true,
        params : JSON.parse(#{(toJSON . encodeParams) t})
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
  return $ if token d == t then return (raw d) else Nothing

_stepResult :: Getter Object (Maybe StepResult)
_stepResult = to parseResult

parseResult :: Object -> Maybe StepResult
parseResult hm = StepResult <$> v "id"
                 <*> v "name"
                 <*> v "basename"
                 <*> v "ext"
                 <*> (parseMIMEType <$> v "mime")
                 <*> v "field"
                 <*> (toURI <$> v "url")
                 <*> (toURI <$> v "ssl_url")
  where v s = case HM.lookup s hm of
          (Just (String t)) -> Just t
          _ -> Nothing
        toURI = parseURI . unpack

-- | Helper method to pull the nth @StepResult@ for a given key from the Transloadit response
nthStepResult :: AsValue s => Maybe s -> Text -> Int -> Maybe StepResult
nthStepResult Nothing _ _ = Nothing
nthStepResult (Just u) k i = u ^? AL.key "results"
                             . AL.key k
                             . nth i
                             . _Object
                             . _stepResult
                             & join

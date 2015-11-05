{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Codec.MIME.Parse
import           Data.Aeson
import           Data.Map
import           Data.Maybe
import           Data.Text
import           System.Locale
import           Test.Hspec
import           Yesod             hiding (Key, get)
import           Yesod.Form.Jquery (YesodJquery (..))
import           Yesod.Test
import           Yesod.Transloadit

data Test = Test
mkYesod "Test" [parseRoutes| / HomeR GET |]

instance Yesod Test
instance YesodJquery Test
instance YesodTransloadit Test

instance RenderMessage Test FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  let now = UTCTime (ModifiedJulianDay 50000) (secondsToDiffTime 10)

  -- Create an id for your form
  ident <- newIdent

  -- Create some Transloadit params, you need: Expiry time; Api key; Template Id; Form id
  let expiry = addUTCTime 3600 now
      key = Key "my_key"
      template = Template "my_template"
      secret = Secret "my_secret"
      params = mkParams expiry key template ident secret

  -- Load the widget, and retrieve the given signature
  sig <- either (const $ error "nooo") transloadIt params

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

sampleDict :: Map Text (Map Text [Map Text Text])
sampleDict = fromList [("results", results)]
             where results = fromList [("foo", stepResults)]
                   stepResults = [fromList [("id","<id>"), ("name", "n"), ("basename", "b"), ("ext", "e"), ("mime", "text/plain"), ("field", "f"), ("url", "u"), ("ssl_url", "<ssl_url>")]]

sampleResult = fromJust $ nthStepResult 0 "foo" (return $ encode sampleDict)

formGenSpecs :: Spec
formGenSpecs = yesodSpec Test $ do
  ydescribe "Form generation" $ do
    yit "adds correct Transloadit params" $ do
      get HomeR
      bodyContains "params : JSON.parse(\"{\\\"auth\\\":{\\\"expires\\\":\\\"1995/10/10 01:00:10+00:00\\\",\\\"key\\\":\\\"my_key\\\"},\\\"template_id\\\":\\\"my_template\\\"}\")"
    yit "computes the correct signature" $ do
      get HomeR
      bodyContains "<input type=\"hidden\" name=\"signature\" value=\"ad2784ec20c0f2d486125141409763ea603e1a10\">"
    yit "adds JQuery" $ do
      get HomeR
      bodyContains "<script src=\"//ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js\"></script>"
    yit "adds Transloadit" $ do
      get HomeR
      bodyContains "<script src=\"https://assets.transloadit.com/js/jquery.transloadit2-v2-latest.js\"></script>"
  ydescribe "Response parsing" $ do
    yit "grabs ssl_url" $ assertEqual "ssl_url" (sampleResult ^. sslUrl) ("<ssl_url>" :: Text)
    yit "grabs id" $ assertEqual "id" (sampleResult ^. resultId) ("<id>" :: Text)
    yit "grabs name" $ assertEqual "name" (sampleResult ^. name) ("n" :: Text)
    yit "grabs basename" $ assertEqual "basename" (sampleResult ^. baseName) ("b" :: Text)
    yit "grabs extension" $ assertEqual "ext" (sampleResult ^. extension) ("e" :: Text)
    yit "grabs mime" $ assertEqual "mime" (sampleResult ^. mime) (parseMIMEType "text/plain")
    yit "grabs field" $ assertEqual "field" (sampleResult ^. field) ("f" :: Text)
    yit "grabs url" $ assertEqual "url" (sampleResult ^. url) ("u" :: Text)

main = hspec formGenSpecs

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Data.Text
import           Data.Time
import           System.Locale
import           Test.Hspec
import           Web.Transloadit
import           Yesod             hiding (Key, get)
import           Yesod.Form.Jquery (YesodJquery (..))
import           Yesod.Test

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

formGenSpecs :: Spec
formGenSpecs = yesodSpec Test $ do
  ydescribe "Form generation" $ do
    yit "adds correct Transloadit params" $ do
      get HomeR
      bodyContains "params : JSON.parse('{\"auth\":{\"expires\":\"1995/10/10 01:00:10+00:00\",\"key\":\"my_key\"},\"template_id\":\"my_template\"}')"
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
    yit "works" $ do
      let sample = Just "{\"results\":{\"foo\":[{\"ssl_url\":\"bar\"}]}}" :: Maybe Text
      assertEqual "Basic example" (extractFirstResult "foo" sample) (Just (String "bar"))

main = hspec formGenSpecs

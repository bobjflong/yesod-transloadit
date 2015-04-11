# transloadit-yesod [![Build Status](https://travis-ci.org/bobjflong/transloadit-yesod.svg?branch=master)](https://travis-ci.org/bobjflong/transloadit-yesod)

This is a reusable Yesod widget for the [Transloadit](https://transloadit.com/) web service.
This widget:

* Injects Javascript dependencies into your frontend
* Computes & injects the Transloadit snippet
* Computes a server side signature for Transloadit's signature authentication
* Parses responses from Transloadit

Here's an example, using Transloadit to crop an uploaded image:

```haskell
-- Make a Yesod app

data Test = Test
mkYesod "Test" [parseRoutes| / HomeR GET POST |]

instance Yesod Test
instance YesodJquery Test
instance YesodTransloadit Test

instance RenderMessage Test FormMessage where
  renderMessage _ _ = defaultFormMessage
```

We use an [Input form](http://www.yesodweb.com/book/forms#forms_input_forms) for flexibility. This
means we need to look after [CSRF](http://en.wikipedia.org/wiki/Cross-site_request_forgery)
ourselves.

```haskell
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
      params = mkParams expiry key template ident secret

  -- Load the widget, and retrieve the given signature
  sig <- either (const $ error "nooo") transloadIt params

  -- CSRF considerations, tokenText is a helper that tries to extract the current CSRF token
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
```

The handler for our form is quite simple, we try to parse the results (using the `extractFirstResult` helper) and present an image:

```haskell
postHomeR :: Handler Html
postHomeR = defaultLayout $ do
  results <- handleTransloadit -- "results" is just JSON, we can use extractFirstResult to optionally parse it

  -- my_template contains a step called "cropped_thumb"
  case extractFirstResult "cropped_thumb" results of
    Just (String url) -> [whamlet| <img src="#{url}"/> |]
    _ -> [whamlet| No results :( |]

  return ()
```

Run it!

```haskell
exampleServer = warp 4567 Test
```

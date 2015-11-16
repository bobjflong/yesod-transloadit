module Yesod.Transloadit.Internal where

import           Data.Text

csrfMatches :: Text -> Text -> Bool
csrfMatches x y = x == y && x /= mempty

{-# LANGUAGE OverloadedStrings #-}

module Yesod.Transloadit.OrderedJSON (
    encode,
    is,
    obj,
    str
  ) where

import           Data.Monoid (mconcat)
import           Data.Text

type KeyValue = (Text, OrderedValue)

data OrderedValue = Object [KeyValue] | String Text deriving (Eq, Show)

quote :: Text
quote = "\""

lbrace :: Text
lbrace = "{"

rbrace :: Text
rbrace = "}"

colon :: Text
colon = ":"

comma :: Text
comma = ","

encodeKV :: KeyValue -> Text
encodeKV (t, v) = mconcat [quote, t, quote, colon, encode v]

encode :: OrderedValue -> Text
encode (String t) = mconcat [quote, t, quote]
encode (Object kvs) = mconcat [lbrace, intercalate comma $ fmap encodeKV kvs, rbrace]

is :: Text -> OrderedValue -> KeyValue
is = (,)

obj :: [KeyValue] -> OrderedValue
obj = Object

str :: Text -> OrderedValue
str = String

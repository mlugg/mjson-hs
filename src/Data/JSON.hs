{-|

Module     : Data.JSON
Copyright  : © Matthew Lugg, 2020
License    : Unlicense
Maintainer : mlugg@mlugg.co.uk
Stability  : experimental

This module defines the JSONElement datatype, which can represent any
JSON value.

-}

{-# LANGUAGE Safe, LambdaCase, OverloadedStrings #-}

module Data.JSON (JSONElement(..)) where

import Data.Text(Text)

-- |'JSONElement' is any value representable in JSON.
data JSONElement
  = JSONObject [(Text, JSONElement)]  -- ^ A JSON object
  | JSONArray [JSONElement]           -- ^ A JSON array
  | JSONString Text                   -- ^ A JSON string (encoded using 'Text')
  | JSONBool Bool                     -- ^ A JSON boolean
  | JSONNum (Either Integer Double)   -- ^ A JSON number (stored as either an 'Integer' or a 'Double')
  | JSONNull                          -- ^ A JSON null value
  deriving (Show)

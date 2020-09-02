{-|

Module     : Data.JSON.Serialize
Copyright  : © Matthew Lugg, 2020
License    : Unlicense
Maintainer : mlugg@mlugg.co.uk
Stability  : experimental

This module defines functions for serializing 'JSONElement's into 'Text' strings.

-}

{-# LANGUAGE Safe, LambdaCase, OverloadedStrings #-}

module Data.JSON.Serialize
  ( serializeJSON
  , serializeJSON' ) where

import Data.JSON
import Data.Serialize
import Data.Text(Text)
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Serialize.Text as S
import Numeric(showHex)
import Data.Serialize.Combinators

-- |'serializeJSON' is a serializer from a 'JSONElement' to the 'Text'
-- type.
serializeJSON :: Serialize Text JSONElement
serializeJSON = Serialize $ \case
  JSONObject x -> runSerializer serObject x
  JSONArray  x -> runSerializer serArray  x
  JSONString x -> runSerializer serString x
  JSONBool   x -> runSerializer serBool   x
  JSONNum    x -> runSerializer serNum    x
  JSONNull     -> runSerializer serNull   ()
  where
    serObject = between (S.lit "{") (S.lit "}") $
      (quoted >*< (S.lit ": " *< serializeJSON)) `sepBy` S.lit ", "

    serArray = between (S.lit "[") (S.lit "]") $
      serializeJSON `sepBy` S.lit ", "

    serString = quoted

    serBool = (\x -> if x then "true" else "false") >$< S.text

    serNum = S.serShow >|< S.serShow

    serNull = S.lit "null"

    quoted = between (S.lit "\"") (S.lit "\"") $
      escape >$< S.text

    escape = T.concatMap escapeChar
    escapeChar c = fromMaybe (if isPrint c then T.singleton c else c') $ lookup c escs
      where c' = "\\u" <> T.justifyRight 4 '0' (T.pack $ showHex (fromEnum c) "")
    escs =
      [ ('"', "\\\"")
      , ('\\', "\\\\")
      , ('\b', "\\b")
      , ('\f', "\\f")
      , ('\n', "\\n")
      , ('\r', "\\r")
      , ('\t', "\\t") ]

-- |'serializeJSON'' is a convenience function which uses the
-- 'serializeJSON' serializer on its argument.
serializeJSON' :: JSONElement -> Text
serializeJSON' = runSerializer serializeJSON

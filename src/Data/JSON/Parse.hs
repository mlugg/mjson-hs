{-|

Module     : Data.JSON.Parse
Copyright  : © Matthew Lugg, 2020
License    : Unlicense
Maintainer : mlugg@mlugg.co.uk
Stability  : experimental

This module defines functions for parsing JSON strings using Megaparsec.

-}

{-# LANGUAGE Trustworthy, OverloadedStrings #-}

module Data.JSON.Parse
  ( parseJSON
  , parseJSON' ) where

import Data.JSON

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos(SourcePos)

import Data.Void
import Data.Text(Text)
import qualified Data.Text as T

-- |'parseJSON' is a Megaparsec parser for JSON values.
parseJSON :: Parsec Void Text JSONElement
parseJSON = sc *> pJSON <* eof

-- |'parseJSON'' is a convenience function which uses the 'parseJSON'
-- parser on its argument.
parseJSON' :: Text -> Either (ParseErrorBundle Text Void) JSONElement
parseJSON' = parse parseJSON "<json text>"

-- Utility parsers {{{

sc = L.space space1 empty empty
symbol = L.symbol sc
comma = symbol ","
colon = symbol ":"

-- }}}

-- JSON parsers {{{

pJSON = pObject
    <|> pArray
    <|> pJString
    <|> pBool
    <|> pNum
    <|> pNull

pObject = between (symbol "{") (symbol "}") $
  fmap JSONObject $
  field `sepBy` comma
  where field = (,) <$> pString <*> (colon *> pJSON)

pArray = between (symbol "[") (symbol "]") $
  fmap JSONArray $
  pJSON `sepBy` comma

pString = T.pack <$> between (char '"') (char '"') (many $ noneOf ['\\', '"'])
pJString = JSONString <$> pString

pBool = fmap JSONBool $
      True  <$ symbol "true"
  <|> False <$ symbol "false"

pNum = undefined

pNull = JSONNull <$ symbol "null"

-- }}}

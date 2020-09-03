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
import Data.Char
import Data.Functor
import Data.Text(Text)
import qualified Data.Text as T
import Data.List

-- |'parseJSON' is a Megaparsec parser for JSON values.
parseJSON :: Parsec Void Text JSONElement
parseJSON = sc *> pJSON <* eof

-- |'parseJSON'' is a convenience function which uses the 'parseJSON'
-- parser on its argument.
parseJSON' :: Text -> Either (ParseErrorBundle Text Void) JSONElement
parseJSON' = parse parseJSON "<json text>"

-- Utility parsers {{{

sc = L.space space1 empty empty
lexeme = L.lexeme sc
symbol = L.symbol sc
comma = symbol ","
colon = symbol ":"
float = lexeme $ L.signed sc L.float
decimal = lexeme $ L.signed sc L.decimal

-- }}}

-- JSON parsers {{{

pJSON = JSONObject <$> pObject
    <|> JSONArray  <$> pArray
    <|> JSONString <$> pString
    <|> JSONBool   <$> pBool
    <|> JSONFloat  <$> pFloat
    <|> JSONInt    <$> pInt
    <|> JSONNull   <$  pNull

pObject = between (symbol "{") (symbol "}") $
  field `sepBy` comma
  where field = (,) <$> pString <*> (colon *> pJSON)

pArray = between (symbol "[") (symbol "]") $
  pJSON `sepBy` comma

pString = lexeme $
  fmap T.pack $
  between (char '"') (char '"') $
  many jsonChar
  where
    jsonChar = single '\\' *> escape
           <|> anySingleBut '"'

    escape = single '"'
         <|> single '\\'
         <|> single '/'
         <|> single 'b' $> '\b'
         <|> single 'f' $> '\f'
         <|> single 'n' $> '\n'
         <|> single 'r' $> '\r'
         <|> single 't' $> '\t'
         <|> single 'u' *> uEsc

    uEsc = toEnum . mkNum <$> count 4 (satisfy isHexDigit)
      where mkNum = foldl' step 0
            step s x = s*16 + fromIntegral (digitToInt x)

pBool = True  <$ symbol "true"
    <|> False <$ symbol "false"

pFloat = try float

pInt = decimal

pNull = symbol "null"

-- }}}

module Main where

import Control.Applicative (Alternative (empty, (<|>)), many)
import Data.Char (isDigit, isSpace)
import Data.Tuple (swap)
import MyParser
import System.IO
import Text.Read (readMaybe)

data JSONValue
  = JSONNumber !Int
  | JSONString !String
  | JSONNull
  | JSONBool !Bool
  | JSONArray ![JSONValue]
  | JSONObject ![(String, JSONValue)]
  deriving (Show)

-- instance Show JSONValue where
--   show = showIdent 2
--     where
--       showIdent _ (JSONNumber n) = show n
--       showIdent _ (JSONString s) = show s
--       showIdent _ JSONNull = "null"
--       showIdent _ (JSONBool x) = show x
--       showIdent _ (JSONArray x) = show x
--       showIdent k (JSONObject x) = "{\n" ++ showList (k + 2) x ++ "\n" ++ replicate (k -2) ' ' ++ "}"
--         where
--           showList k [] = replicate k ' '
--           showList k [(prop, val)] = replicate k ' ' ++ show prop ++ " : " ++ show val
--           showList k ((prop, val) : xs) = replicate k ' ' ++ show prop ++ " : " ++ show val ++ ",\n" ++ showList k xs

parseNull :: Parser JSONValue
parseNull = JSONNull <$ stringP "null"

parseBool :: Parser JSONValue
parseBool = fmap JSONBool $ (True <$ stringP "true") <|> (False <$ stringP "false")

parseNumber :: Parser JSONValue
parseNumber = flattenParser $ f <$> spanP isDigit
  where
    f s = JSONNumber <$> (readMaybe s :: Maybe Int)

parseString :: Parser JSONValue
parseString =
  JSONString <$> do
    charP '"'
    spanQuoteP

parseArrayComma :: Parser JSONValue
parseArrayComma = do
  charP ','
  parseValue

parseArray :: Parser JSONValue
parseArray =
  fmap JSONArray $
    ( do
        charP '['
        x <- parseValue
        xs <- many parseArrayComma
        charP ']'
        return $ x : xs
    )
      <|> do
        charP '['
        charP ']'
        return []

parseObjectPair :: Parser (String, JSONValue)
parseObjectPair = do
  charP '"'
  key <- spanQuoteP
  charP ':'
  value <- parseValue
  return (key, value)

parseObject :: Parser JSONValue
parseObject =
  JSONObject <$> do
    charP '{'
    x <- parseObjectPair
    xs <- many parseObjectComma
    charP '}'
    return $ x : xs
  where
    parseObjectComma = do
      charP ','
      parseObjectPair

parseValue :: Parser JSONValue
parseValue = parseArray <|> parseObject <|> parseString <|> parseNumber <|> parseBool <|> parseNull

main :: IO ()
main = do
  handle <- openFile "test.json" ReadMode
  contents <- hGetContents handle
  let parsed = runParser parseValue contents
  print (snd $ treat parsed)
  hClose handle
  where
    treat (Just x) = x
    treat _ = error "Invalid JSON file"
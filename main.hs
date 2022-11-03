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

instance Show JSONValue where
  show = showIdent 2
    where
      showIdent _ (JSONNumber n) = show n
      showIdent _ (JSONString s) = show s
      showIdent _ JSONNull = "null"
      showIdent _ (JSONBool x) = show x
      showIdent _ (JSONArray x) = show x
      showIdent k (JSONObject x) = "{\n" ++ showList (k + 2) x ++ "\n" ++ replicate (k -2) ' ' ++ "}"
        where
          showList k [] = replicate k ' '
          showList k [(prop, val)] = replicate k ' ' ++ show prop ++ " : " ++ show val
          showList k ((prop, val) : xs) = replicate k ' ' ++ show prop ++ " : " ++ show val ++ ",\n" ++ showList k xs

parseNull :: Parser JSONValue
parseNull = JSONNull <$ stringP "null"

parseBool :: Parser JSONValue
parseBool = fmap JSONBool $ (True <$ stringP "true") <|> (False <$ stringP "false")

parseNumber :: Parser JSONValue
parseNumber = flattenParser $ f <$> spanP isDigit
  where
    f s = JSONNumber <$> (readMaybe s :: Maybe Int)

parseString :: Parser JSONValue
parseString = JSONString <$> do
  charP '"'
  spanQuoteP

parseArrayComma :: Parser JSONValue
parseArrayComma = do
  wsP
  charP ','
  wsP
  parseValue

parseArray :: Parser JSONValue
parseArray = fmap JSONArray $ (do
  charP '['
  wsP
  x <- parseValue
  xs <- many parseArrayComma
  wsP
  charP ']'
  return $ x:xs) <|> do
  charP '['
  wsP
  charP ']'
  return []
  

parseObjectPair :: Parser (String, JSONValue)
parseObjectPair = do
  wsP
  charP '"'
  key <- spanQuoteP
  wsP
  charP ':'
  wsP
  value <- parseValue
  return (key, value)

parseObject :: Parser JSONValue
parseObject = JSONObject <$> do
  wsP
  charP '{'
  x <- parseObjectPair
  xs <- many parseObjectComma
  wsP
  charP '}'
  return $ x:xs 
  where 
    parseObjectComma = do
      wsP
      charP ','
      wsP
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
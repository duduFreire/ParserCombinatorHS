{-# LANGUAGE InstanceSigs #-}
module Main where

import MyParser
import System.IO
import Control.Applicative ( Alternative((<|>), empty), many)
import Data.Tuple ( swap )
import Data.Char ( isDigit, isSpace )
import Text.Read (readMaybe)

data JSONValue = JSONNumber !Int | JSONString !String | JSONNull | JSONBool !Bool |
                 JSONArray ![JSONValue] | JSONObject ![(String, JSONValue)] 

instance Show JSONValue where
    show = showIdent 2 where
        showIdent _ (JSONNumber n) = show n
        showIdent _ (JSONString s) = show s
        showIdent _ JSONNull = "null"
        showIdent _ (JSONBool x) = show x
        showIdent _ (JSONArray x) = show x
        showIdent k (JSONObject x) = "{\n" ++ (showList (k+2) x) ++ "\n" ++ replicate (k-2) ' ' ++ "}" where
            showList k [] = replicate k ' '
            showList k [(prop, val)] = replicate k ' ' ++ (show prop) ++ " : " ++ (show val)
            showList k ((prop,val):xs) = replicate k ' ' ++  show prop ++ " : " ++ show val ++ ",\n" ++ (showList k xs)

parseNull :: Parser JSONValue
parseNull = Parser $ \s -> do
    (rest, _) <- runParser (stringP "null") s
    return (rest, JSONNull)

parseBool :: Parser JSONValue
parseBool = JSONBool . f <$> (stringP "true" <|> stringP "false") where
        f "true" = True
        f "false" = False
        -- TODO: make this better
        f _ = undefined

parseNumber :: Parser JSONValue
parseNumber = flattenParser $ f <$> spanP isDigit where
    f s = JSONNumber <$> (readMaybe s :: Maybe Int)

parseString :: Parser JSONValue
parseString = fmap JSONString $ charP '"' *> spanQuoteP

parseArrayComma :: Parser JSONValue
parseArrayComma = wsP *> charP ',' *> wsP *> parseValue

parseArray :: Parser JSONValue
parseArray = (fmap JSONArray $ charP '[' *> wsP *> ((:) <$> parseValue)
   <*> many parseArrayComma <* wsP <* charP ']') <|>
   (fmap (const (JSONArray [])) $ charP '[' *> wsP <* charP ']')

parseObjectPair :: Parser (String, JSONValue)
parseObjectPair = (wsP *> charP '"' *> ((,) <$> spanQuoteP)) <*>
    (wsP *> charP ':' *> wsP *> parseValue)

parseEmptyString :: Parser JSONValue
parseEmptyString = JSONString <$> stringP "\"\""

parseObject :: Parser JSONValue
parseObject = fmap JSONObject $ wsP *> charP '{' *> ((:) <$> parseObjectPair) <*>
 (many (wsP *> charP ',' *> wsP *> parseObjectPair)) <* wsP <* charP '}'


parseValue :: Parser JSONValue
parseValue = parseArray <|> parseObject <|> parseString <|> parseNumber <|> parseBool <|> parseNull

forceMaybe :: Maybe a -> a
forceMaybe (Just x) = x
forceMaybe Nothing = undefined

main = do  
    handle <- openFile "test.json" ReadMode  
    contents <- hGetContents handle 
    let parsed = runParser parseValue contents
    putStrLn (show $ snd $ treat parsed)
    hClose handle where
        treat (Just x) = x
        treat _ = error "Invalid JSON file"
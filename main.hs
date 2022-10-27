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
        showIdent k (JSONObject x) = "{\n" ++ showList (k+2) x ++ "\n" ++ replicate (k-2) ' ' ++ "}" where
            showList k [] = replicate k ' '
            showList k [(prop, val)] = replicate k ' ' ++ show prop ++ " : " ++ show val
            showList k ((prop,val):xs) = replicate k ' ' ++  show prop ++ " : " ++ show val ++ ",\n" ++ showList k xs

parseNull :: MyParser.Parser JSONValue
parseNull = MyParser.Parser $ \s -> do
    (rest, _) <- MyParser.runParser (MyParser.stringP "null") s
    return (rest, JSONNull)

parseBool :: MyParser.Parser JSONValue
parseBool = JSONBool . f <$> (MyParser.stringP "true" <|> MyParser.stringP "false") where
        f "true" = True
        f "false" = False
        -- TODO: make this better
        f _ = undefined

parseNumber :: MyParser.Parser JSONValue
parseNumber = MyParser.flattenParser $ f <$> MyParser.spanP isDigit where
    f s = JSONNumber <$> (readMaybe s :: Maybe Int)

parseString :: MyParser.Parser JSONValue
parseString = fmap JSONString $ MyParser.charP '"' *> MyParser.spanQuoteP

parseArrayComma :: MyParser.Parser JSONValue
parseArrayComma = MyParser.wsP *> MyParser.charP ',' *> MyParser.wsP *> parseValue

parseArray :: MyParser.Parser JSONValue
parseArray = fmap JSONArray (MyParser.charP '[' *> MyParser.wsP *> ((:) <$> parseValue)
   <*> many parseArrayComma <* MyParser.wsP <* MyParser.charP ']') <|>
   fmap (const (JSONArray [])) (MyParser.charP '[' *> MyParser.wsP <* MyParser.charP ']')

parseObjectPair :: MyParser.Parser (String, JSONValue)
parseObjectPair = (MyParser.wsP *> MyParser.charP '"' *> ((,) <$> MyParser.spanQuoteP)) <*>
    (MyParser.wsP *> MyParser.charP ':' *> MyParser.wsP *> parseValue)

parseEmptyString :: MyParser.Parser JSONValue
parseEmptyString = JSONString <$> MyParser.stringP "\"\""

parseObject :: MyParser.Parser JSONValue
parseObject = fmap JSONObject $ MyParser.wsP *> MyParser.charP '{' *> ((:) <$> parseObjectPair) <*>
 many (MyParser.wsP *> MyParser.charP ',' *> MyParser.wsP *> parseObjectPair) <* MyParser.wsP <* MyParser.charP '}'


parseValue :: MyParser.Parser JSONValue
parseValue = parseArray <|> parseObject <|> parseString <|> parseNumber <|> parseBool <|> parseNull

forceMaybe :: Maybe a -> a
forceMaybe (Just x) = x
forceMaybe Nothing = undefined

main = do
    handle <- openFile "test.json" ReadMode
    contents <- hGetContents handle
    let parsed = MyParser.runParser parseValue contents
    print (snd $ treat parsed)
    hClose handle where
        treat (Just x) = x
        treat _ = error "Invalid JSON file"
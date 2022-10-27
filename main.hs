{-# LANGUAGE InstanceSigs #-}
module Main where

import System.IO
import Control.Applicative ( Alternative((<|>), empty), many)
import Data.Tuple ( swap )
import Data.Char ( isDigit, isSpace )
import Text.Read (readMaybe)

data JSONValue = JSONNumber !Int | JSONString !String | JSONNull | JSONBool !Bool |
                 JSONArray ![JSONValue] | JSONObject ![(String, JSONValue)] deriving Show

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \s -> g s where
        g s = do
            (remaining, parsed) <- runParser p s
            return (remaining, f parsed)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \s -> Just (s, x)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p1 <*> p2 = Parser $ \s -> do
        (rest1, parsed1) <- runParser p1 s
        (rest2, parsed2) <- runParser p2 rest1
        return (rest2, parsed1 parsed2)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

charP :: Char -> Parser Char
charP x = Parser $ \s -> f s where
    f (y:ys) | x == y = Just (ys, y)
    f _ = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \s -> Just $ swap $ span f s

spanQuote :: String -> (String, String)
spanQuote (x:y:ys) | x /= '\\' && y == '"' = ([x], ys)
                   | otherwise = (x:last1, last2) where
                    (last1, last2) = spanQuote (y:ys)
spanQuote _ = ("", "")

spanQuoteP :: Parser String
spanQuoteP = Parser $ \s -> Just $ swap $ spanQuote s

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

flattenParser :: Parser (Maybe a) -> Parser a
flattenParser p1 = Parser $ \s -> do
    (rest, parsed) <- runParser p1 s
    result <- parsed
    return (rest, result)

parseNumber :: Parser JSONValue
parseNumber = flattenParser $ f <$> spanP isDigit where
    f s = JSONNumber <$> (readMaybe s :: Maybe Int)

parseString :: Parser JSONValue
parseString = fmap JSONString $ quoteP *> spanQuoteP where
    quoteP = charP '"'

wsP :: Parser String
wsP = spanP isSpace

parseArrayComma :: Parser JSONValue
parseArrayComma = wsP *> charP ',' *> wsP *> parseValue

parseArray :: Parser JSONValue
parseArray = (fmap JSONArray $ charP '[' *> wsP *> ((:) <$> parseValue)
   <*> many parseArrayComma <* wsP <* charP ']') <|>
   (fmap (const (JSONArray [])) $ charP '[' *> wsP <* charP ']')

parseObjectPair :: Parser (String, JSONValue)
parseObjectPair = (wsP *> charP '"' *> ((,) <$> spanQuoteP)) <*>
    (wsP *> charP ':' *> wsP *> parseValue)

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
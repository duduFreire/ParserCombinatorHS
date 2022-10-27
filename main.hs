{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Applicative ( Alternative((<|>), empty), some, many)
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
parseString = fmap JSONString $ quoteP *> spanP (/= '"') <* quoteP where
    quoteP = charP '"'

wsP :: Parser String
wsP = spanP isSpace

parseArrayComma :: Parser JSONValue
parseArrayComma = wsP *> charP ',' *> wsP *> parseValue

parseArray :: Parser JSONValue
parseArray = fmap JSONArray $ charP '[' *> wsP *> (pure (:) <*> parseValue) <*> many parseArrayComma <* wsP <* charP ']'

parseObject :: Parser JSONValue
parseObject = Parser $ \s -> do
    (rest1, parsed1) <- runParser (charP '{' *> wsP *> charP '"' *> spanP (/= '"') <* charP '"') s
    (rest2, parsed2) <- runParser (wsP *> charP ':' *> wsP *> parseValue <* charP '}') rest1
    return (rest2, JSONObject [(parsed1, parsed2)])


parseValue :: Parser JSONValue
parseValue = parseArray <|> parseObject <|> parseString <|> parseNumber <|> parseBool <|> parseNull

main :: IO ()
main = undefined
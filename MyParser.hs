{-# LANGUAGE InstanceSigs #-}

module MyParser
  ( Parser (..),
    charP,
    stringP,
    spanQuoteP,
    spanP,
    wsP,
    flattenParser,
    charPredP,
    natP,
    iCharP,
    iStringP,
    unpairParser,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isDigit, isSpace, toLower)
import Data.Tuple (swap)
import Text.Read (readMaybe)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \s -> g s
    where
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
charP x = Parser $ \s -> f s
  where
    f (y : ys) | x == y = Just (ys, y)
    f _ = Nothing

iCharP :: Char -> Parser Char
iCharP x = Parser $ \s -> f s
  where
    f (y : ys) | toLower x == toLower y = Just (ys, y)
    f _ = Nothing

natP :: Parser Int
natP = flattenParser $ readMaybe <$> spanP isDigit

stringP :: String -> Parser String
stringP = traverse charP

iStringP :: String -> Parser String
iStringP = traverse iCharP

unpairParser :: Parser (a, b) -> (Parser a, Parser b)
unpairParser p = (x, y)
  where
    x = Parser $ \s ->
      ( do
          (rest, parsed) <- runParser p s
          return (rest, fst parsed)
      )

    y = Parser $ \s -> do
      (rest, parsed) <- runParser p s
      return (rest, snd parsed)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \s -> Just $ swap $ span f s

wsP :: Parser String
wsP = spanP isSpace

spanQuote :: String -> (String, String)
spanQuote (x : ys) | x == '\"' = ("", ys)
spanQuote (x : y : ys)
  | x /= '\\' && y == '"' = ([x], ys)
  | otherwise = (x : last1, last2)
  where
    (last1, last2) = spanQuote (y : ys)
spanQuote _ = ("", "")

spanQuoteP :: Parser String
spanQuoteP = Parser $ \s -> Just $ swap $ spanQuote s

flattenParser :: Parser (Maybe a) -> Parser a
flattenParser p1 = Parser $ \s -> do
  (rest, parsed) <- runParser p1 s
  result <- parsed
  return (rest, result)

charPredP :: (Char -> Bool) -> Parser Char
charPredP f = Parser $ \s -> handle s
  where
    handle "" = Nothing
    handle (x : xs)
      | f x = Just (xs, x)
      | otherwise = Nothing
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Main where

import Control.Applicative (Alternative (empty, (<|>)), many)
import Data.Char (isDigit, isLetter, isSpace)
import Data.Tuple (swap)
import MyParser
import System.IO
import Text.Read (readMaybe)

data LoopProg
  = Assign !String !Int
  | Increment !String
  | Compose !LoopProg !LoopProg
  | Loop !String !LoopProg
  deriving (Show)

parseVariable :: Parser String
parseVariable =
  do
    first <- charPredP isLetter
    rest <- spanP (\x -> isLetter x || isDigit x)
    return $ first : rest

parseAssign :: Parser LoopProg
parseAssign =
  do
    var <- parseVariable
    stringP ":="
    val <- natP
    return $ Assign var val

parseIncrement :: Parser LoopProg
parseIncrement =
  Increment
    <$> flattenParser
      ( do
          var1 <- parseVariable
          stringP ":="
          var2 <- parseVariable
          charP '+'
          charP '1'
          return (if var1 == var2 then Just var1 else Nothing)
      )

parseLoop :: Parser LoopProg
parseLoop =
  do
    iStringP "loop"
    var <- parseVariable
    iStringP "do"
    prog <- parseLoopProg
    iStringP "end"
    return $ Loop var prog

parseSingleProg :: Parser LoopProg
parseSingleProg = parseIncrement <|> parseAssign <|> parseLoop

parseLoopProg :: Parser LoopProg
parseLoopProg = do
  prog <- parseSingleProg
  progs <- many $ charP ';' >> parseSingleProg
  return $ foldr Compose prog progs

main :: IO ()
main = do
  handle <- openFile "tests/test.loop" ReadMode
  contents <- hGetContents handle
  let parsed = runParser parseLoopProg contents
  putStrLn $ treat parsed
  hClose handle
  where
    treat (Just ("", x)) = show x
    treat _ = "Invalid LOOP program"
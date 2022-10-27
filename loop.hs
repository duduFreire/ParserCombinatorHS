{-# LANGUAGE InstanceSigs #-}
module Main where

import MyParser
import System.IO
import Control.Applicative ( Alternative((<|>), empty), many)
import Data.Tuple ( swap )
import Data.Char ( isDigit, isSpace , isLetter)
import Text.Read (readMaybe)

data LoopProg = Assign String | Increment String | Compose LoopProg LoopProg | Loop String LoopProg

parseVariable :: Parser String
parseVariable = ((:) <$> charPredP isLetter) <*> spanP (\x -> isLetter x || isDigit x)

parseAssignOp :: Parser String
parseAssignOp = stringP ":="

parseAssign :: Parser LoopProg
parseAssign = undefined

main = undefined
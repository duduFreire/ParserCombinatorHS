{-# LANGUAGE InstanceSigs #-}
module Main where

import MyParser
import System.IO
import Control.Applicative ( Alternative((<|>), empty), many)
import Data.Tuple ( swap )
import Data.Char ( isDigit, isSpace , isLetter)
import Text.Read (readMaybe)

data LoopProg = Assign String Int | Increment String | 
    Compose LoopProg LoopProg | Loop String LoopProg deriving Show

parseVariable :: Parser String
parseVariable = ((:) <$> charPredP isLetter) <*> spanP (\x -> isLetter x || isDigit x)

parseAssignOp :: Parser String
parseAssignOp = stringP ":="

parseAssign :: Parser LoopProg
parseAssign = fmap (uncurry Assign) $ (wsP *> ((,) <$> parseVariable)) <*> (wsP *> parseAssignOp *> wsP *> natP)


main = undefined
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Main where

import qualified Data.Map as Map
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
  return $ foldl Compose prog progs

main :: IO ()
main= do
  handle <- openFile "tests/test.loop" ReadMode
  varsLine <- getLine
  let maybeVars = readMaybe varsLine :: Maybe [(String, Int)]
  contents <- hGetContents handle
  let parsed = runParser parseLoopProg contents
  putStrLn $ logProgVars maybeVars parsed
  putStrLn $ logProgAST parsed
  hClose handle
  where
    logProgAST (Just ("", x)) = show x
    logProgAST _ = "Invalid LOOP program"
    logProgVars (Just vars) (Just ("", x)) = show $ runLoopProg (Map.fromList vars) x
    logProgVars _ _ = "Invalid LOOP program"


runLoopProg :: Map.Map String Int -> LoopProg -> Map.Map String Int
runLoopProg vars (Assign s i) = Map.union (Map.fromList [(s, i)]) vars
runLoopProg vars (Increment s) = Map.union (Map.fromList [(s, 1 + Map.findWithDefault 0 s vars)]) vars
runLoopProg vars (Compose prog1 prog2) = runLoopProg (runLoopProg vars prog1) prog2
runLoopProg vars (Loop var prog) = loop vars prog (Map.findWithDefault 0 var vars) where
  loop vars prog 0 = vars
  loop vars prog n = runLoopProg (loop vars prog (n-1)) prog 
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
parseVariable = ((:) <$> charPredP isLetter) <*> spanP (\x -> isLetter x || isDigit x)

parseAssignOp :: Parser String
parseAssignOp = stringP ":="

parseAssign :: Parser LoopProg
parseAssign =
  fmap (uncurry Assign) $
    (wsP *> ((,) <$> parseVariable))
      <*> (wsP *> parseAssignOp *> wsP *> natP <* wsP)

parseIncrement :: Parser LoopProg
parseIncrement =
  flattenParser $
    fmap f $
      (wsP *> ((,) <$> parseVariable) <* wsP <* parseAssignOp <* wsP)
        <*> (parseVariable <* wsP <* charP '+' <* wsP <* charP '1' <* wsP)
  where
    f (x, y)
      | x == y = Just $ Increment x
      | otherwise = Nothing

parseLoop :: Parser LoopProg
parseLoop =
  fmap (uncurry Loop) $
    (wsP *> iStringP "loop" *> wsP *> ((,) <$> parseVariable) <* wsP <* iStringP "do" <* wsP)
      <*> (parseLoopProg <* wsP <* iStringP "end" <* wsP)

split :: Char -> String -> (String, String)
split _ [] = ("", "")
split x (y : ys)
  | x == y = ("", ys)
  | otherwise = (y : last1, last2)
  where
    (last1, last2) = split x ys

splitTwice :: Char -> String -> (String, String, String)
splitTwice x s =
  let (sp1, sp1') = split x s; (sp2, sp2') = split x sp1'
   in (sp1, sp2, sp2')

splitP :: Char -> Parser (String, String)
splitP x = Parser $ \s ->
  let (parsed1, parsed2, rest) = splitTwice x s
   in Just (rest, (parsed1, parsed2))

collapseListProg :: LoopProg -> [LoopProg] -> LoopProg
collapseListProg x [] = x
collapseListProg x (y : ys) = Compose x (collapseListProg y ys)

parseSingleProg :: Parser LoopProg
parseSingleProg = parseIncrement <|> parseAssign <|> parseLoop

parseLoopProg :: Parser LoopProg
parseLoopProg =
  fmap (uncurry collapseListProg) $
    ((,) <$> parseSingleProg)
      <*> many (wsP *> charP ';' *> wsP *> parseSingleProg)

main :: IO ()
main = do
  handle <- openFile "test.loop" ReadMode
  contents <- hGetContents handle
  let parsed = runParser parseLoopProg contents
  print (snd $ treat parsed)
  hClose handle
  where
    treat (Just x) = x
    treat _ = error "Invalid LOOP program"
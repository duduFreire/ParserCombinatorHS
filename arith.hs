import Control.Applicative (Alternative (empty, (<|>)), many)
import MyParser

-- expr = term ('+' term | '-' term)*
-- term = factor ('*' term | '/' term)*
-- factor = floatingPointNumber | '(' expr ')'
-- 4 * 5 + 3 = expr (factor 4 [*5]) [+3]
-- Expr (Term (Number 4) [('*', Term (Number 5) [])]) [('+', Term (Number 3) [])]

data TermOp = TermMul | TermDiv

data ExprOp = ExprAdd | ExprSub

data Factor = Number !Float | Factor !Expr deriving (Show)

data Term = Term !Factor ![(TermOp, Term)] deriving (Show)

data Expr = Expr !Term ![(ExprOp, Term)] deriving (Show)

evalFactor :: Factor -> Float
evalFactor (Number x) = x
evalFactor (Factor expr) = evalExpr expr

evalTerm :: Term -> Float
evalTerm (Term x ys) = foldr (\(op, t) acc -> f acc op t) (evalFactor x) ys
  where
    f acc TermMul t = acc * evalTerm t
    f acc TermDiv t = acc / evalTerm t

evalExpr :: Expr -> Float
evalExpr (Expr x ys) = foldr (\(op, t) acc -> f acc op t) (evalTerm x) ys
  where
    f acc ExprAdd t = acc + evalTerm t
    f acc ExprSub t = acc - evalTerm t

instance Show TermOp where
  show TermMul = "*"
  show TermDiv = "/"

instance Show ExprOp where
  show ExprAdd = "+"
  show ExprSub = "-"

-- instance Show Factor where
--   show (Number x) = show x
--   show (Factor x) = ['('] ++ show x ++ [')']

-- instance Show Term where
--   show (Term x []) = show x
--   show (Term x ((op, y) : ys)) = show x ++ show op ++ show y ++ (case ys of [] -> []; _ -> show ys)

-- instance Show Expr where
--   show (Expr x []) = show x
--   show (Expr x ((op, y) : ys)) = show x ++ show op ++ show y ++ (case ys of [] -> []; _ -> show ys)

factorP :: Parser Factor
factorP =
  Number <$> floatP
    <|> Factor
    <$> do
      charP '('
      expr <- exprP
      charP ')'
      return expr

termP :: Parser Term
termP =
  uncurry Term <$> do
    factor <- factorP
    list <- many interiorTermP
    return (factor, list)
  where
    interiorTermP = do
      op <- TermMul <$ charP '*' <|> TermDiv <$ charP '/'
      term <- termP
      return (op, term)

exprP :: Parser Expr
exprP =
  uncurry Expr <$> do
    term <- termP
    list <- many interiorExprP
    return (term, list)
  where
    interiorExprP = do
      op <- ExprAdd <$ charP '+' <|> ExprSub <$ charP '-'
      term <- termP
      return (op, term)

main = do
  line <- getLine
  let result = runParser exprP line
  case result of
    Just ("", expr) -> print expr
    _ -> print "Failed to parse expression."
  main

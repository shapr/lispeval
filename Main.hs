{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text

{-
from https://ccrma.stanford.edu/courses/124/resources/Basic%20Lisp%20Primitives.htm
(+ number &rest numbers) (- number &rest numbers) (* number &rest numbers) (/ number &rest numbers)

(= number &rest numbers) (< number &rest numbers) (> number &rest numbers) (<= number &rest numbers) (>= number &rest numbers)
-}

data Expr = Lit Integer -- does not handle negative numbers!
          -- arithmetic operators
          | Add Expr Expr -- (+ 1 1)
          | Divide Expr Expr -- (/ 4 2)
          | Multiply Expr Expr -- (* 2 2)
          | Subtract Expr Expr -- (- 2 1)
          -- arithmetic relations
          | PEq Expr Expr -- (= 1 1)
          | PLt Expr Expr -- (< 1 2)
          | PGt Expr Expr -- (> 2 1)
          | PLtEq Expr Expr -- (<= 1 2)
          | PGtEq Expr Expr -- (>= 2 1)
          -- predicates
          | PNumber Expr -- (numberp 1)
          | PFloat Expr -- (floatp 1.0)
          | PZero Expr -- (zerop 0)
          | PPlus Expr -- (plusp 1)
          | PMinus Expr -- (minusp -1)
          | POdd Expr -- (oddp 3)
          | PEven Expr -- (evenp 2)
            -- skipping "other numeric functions" for now
          | List [Expr]
            deriving Show

-- there must be some way to factor out this code to make it simpler, not sure how
-- real lisp handles any number of args, pExpr `sepBy` char ' ' ?
pExpr :: Parser Expr
pExpr = Lit <$> decimal -- does not handle negative numbers
        -- <|> Add <$ char '(' <* char '+' <* space <*> pExpr <* space <*> pExpr <* char ')'
        <|> Add <$ left '+' <*> pExpr <* space <*> pExpr <* char ')'
        <|> Divide <$ left '/' <*> pExpr <* space <*> pExpr <* char ')'
        <|> Multiply <$ left '*' <*> pExpr <* space <*> pExpr <* char ')'
        <|> Subtract <$ left '-' <*> pExpr <* space <*> pExpr <* char ')'
        <|> PLt <$ left '<' <*> pExpr <* space <*> pExpr <* char ')'
        <|> PGt <$ left '>' <*> pExpr <* space <*> pExpr <* char ')'
        <|> PLtEq <$ char '(' <* string "<=" <* space <*> pExpr <* space <*> pExpr <* char ')'
        <|> PGtEq <$ char '(' <* string ">=" <* space <*> pExpr <* space <*> pExpr <* char ')'
        <|> PNumber <$ char '(' <* string "numberp" <* space <*> pExpr <* char ')'
        <|> PFloat <$ char '(' <* string "floatp" <* space <*> pExpr <* char ')'
        <|> PZero <$ char '(' <* string "zerop" <* space <*> pExpr <* char ')'
        <|> PPlus <$ char '(' <* string "plusp" <* space <*> pExpr <* char ')'
        <|> PMinus <$ char '(' <* string "minusp" <* space <*> pExpr <* char ')'
        <|> POdd <$ char '(' <* string "oddp" <* space <*> pExpr <* char ')'
        <|> PEven <$ char '(' <* string "evenp" <* space <*> pExpr <* char ')'
            -- why doesn't this same thing work for Add, etc?
        <|> List <$ char '(' <* string "list" <*> exprs <* char ')'
            where exprs = many (space *> pExpr)
                  left c = char '(' <* char c <* space


pEval :: Expr -> Expr
pEval l@(Lit _) = l
pEval (Add a b) = case (pEval a, pEval b) of
                    (Lit a',Lit b') -> Lit (a' + b') -- structural recursion
-- pEval (Add (Lit a) (Lit b)) = Lit (a + b) -- base case for add
-- pEval (Add a b) | Lit a' <- pEval a, Lit b' <- pEval b = Lit (a' + b')
-- pEval (Add a@(Add _ _) b) = Add (pEval a) b
-- pEval (Add a b@(Add _ _)) = Add a (pEval b)
-- pEval (Add a b) =
-- (Add (Add a b) c) -> (Add (a + b) c)

{-
is an expression just a reduced term?
is a value separate from an expression?

a redex is a value that can be reduced

cdsmithus exclaims that if a value is the same type as an expression, it gets messy

-}
fromRight (Right r) = r

main :: IO ()
main = do
  print "Hello, Haskell lisp parser!"
  print $ parseOnly pExpr "(list (+ (- 1 1) (* 1 (- 2 3))))"
  print "Hello, Haskell lisp evaluator!"
  print $ Prelude.take 4 $ iterate pEval $ fromRight $ parseOnly pExpr "(+ (+ 1 1) (+ 1 1))"

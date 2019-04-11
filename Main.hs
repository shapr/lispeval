{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
        <|> Add <$ char '(' <* char '+' <* space <*> pExpr <* space <*> pExpr <* char ')'
        <|> Divide <$ char '(' <* char '/' <* space <*> pExpr <* space <*> pExpr <* char ')'
        <|> Multiply <$ char '(' <* char '*' <* space <*> pExpr <* space <*> pExpr <* char ')'
        <|> Subtract <$ char '(' <* char '-' <* space <*> pExpr <* space <*> pExpr <* char ')'
        <|> PLt <$ char '(' <* char '<' <* space <*> pExpr <* space <*> pExpr <* char ')'
        <|> PGt <$ char '(' <* char '>' <* space <*> pExpr <* space <*> pExpr <* char ')'
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

main :: IO ()
main = do
  print "Hello, Haskell lisp parser!"
  print $ parseOnly pExpr "(list (+ 1 (- 2 3)))"


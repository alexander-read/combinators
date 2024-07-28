{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE LambdaCase #-}

----------------------------------------------------------------------------
-- |
-- Module      : Combinators.Parser
-- Description : A parser for combinatory and lambda expressions
--
----------------------------------------------------------------------------
module Combinators.Parser
  ( parseExpr
  , parseLExpr
  , runParser
  ) where

import Combinators.Calculus
import Combinators.Lambda

import Data.Bifunctor ( first )
import Data.List ( singleton )

import Control.Applicative ( Alternative )
import Control.Applicative.Combinators ( many, sepBy )
import Control.Monad.Combinators ( between, (<|>), empty )

----------------------------------------------------------------------------
---- * Parser Combinators

newtype Parser a = Parser {
    runParser :: String -> Either ParserError (a, String)
}

data ParserError = Unexpected Char | UnexpectedEOF

instance Functor Parser where
  fmap f l = Parser $ (fmap (first f)) . (runParser l)

instance Applicative Parser where
  pure c    = Parser $ \cs -> Right (c,cs)
  pf <*> px = Parser $ \cs -> do
    (f', rest)  <- runParser pf cs
    (p', rest') <- runParser px rest
    return (f' p', rest')

instance Alternative Parser where
  empty   = Parser $ Left . parseError
  p <|> q = Parser $ \cs -> case (runParser p cs, runParser q cs) of
    (result, Left _) -> result
    (Left _, result) -> result
    (a@(Right (_, restA)), b@(Right (_, restB))) ->
      if length restA <= length restB then a else b

instance Monad Parser where
  return  = pure
  p >>= f = Parser $ \cs -> do
    (a, cs')   <- runParser p cs
    (a', rest) <- runParser (f a) cs'
    return (a', rest)

parseError :: String -> ParserError
parseError xs = case xs of
  []    -> UnexpectedEOF
  (x:_) -> Unexpected x

item :: Parser Char
item = Parser $ \case
    ""     -> Left (parseError "")
    (c:cs) -> Right (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= cond
  where
    cond :: Char -> Parser Char
    cond c = if p c then return c else empty

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string ""     = return ""
string (x:xs) = char x >> string xs >> return (x:xs)

space :: Parser String
space = many $ satisfy isSpace
  where
    isSpace s = (s == ' ' || s == '\n' || s == '\t')

token :: Parser a -> Parser a
token p = p <* space >>= return

symbol :: String -> Parser String
symbol = token . string

atom :: Parser String
atom = do {var <- token letter; singleton <$> return var}
  where
    letter = satisfy (`elem` ['a'..'z'])

----------------------------------------------------------------------------
---- * Parse Lambda Expressions

parseLExpr :: Parser (LExpr String)
parseLExpr = parseLApp <|> parseLambda <|> parseLVar

-- For parsing we want to make sure that variables
-- are based on the number of binders etc.
parseLVar :: Parser (LExpr String)
parseLVar = space *> (LVar <$> atom) <* space

parseLambda :: Parser (LExpr String)
parseLambda = do
  var  <- symbol "\\" *> atom <* symbol "."
  expr <- parseLExpr
  return (Lam var expr)

parseLApp :: Parser (LExpr String)
parseLApp = foldl1 App <$> (parseNested <|> parseFlat) `sepBy` space
    where
      parseFlat   = parseLVar <|> parseLambda
      parseNested = parens parseLApp
      parens      = between (symbol "(") (symbol ")")

----------------------------------------------------------------------------
---- * Parse Combinatory Expressions

parseExpr :: Parser (Expr String)
parseExpr = parsePrefixExpr <|> parseInfixExpr

parseInfixExpr :: Parser (Expr String)
parseInfixExpr  = parseInfixApp <|> parseCom <|> parseVar

parsePrefixExpr :: Parser (Expr String)
parsePrefixExpr = parsePrefixApp <|> parseCom <|> parseVar

parseCom :: Parser (Expr String)
parseCom = Com <$> parseConst
  where
    parseConst = (symbol "S"  >> return S)
             <|> (symbol "K"  >> return K)
             <|> (symbol "I"  >> return I)
             <|> (symbol "B"  >> return B)
             <|> (symbol "C"  >> return C)
             <|> (symbol "W"  >> return W)
             <|> (symbol "S'" >> return S')
             <|> (symbol "B'" >> return B')
             <|> (symbol "C'" >> return C')

parseVar :: Parser (Expr String)
parseVar = space *> (Var <$> atom) <* space

parseInfixApp :: Parser (Expr String)
parseInfixApp = foldl1 (:@) <$> (parseNested <|> parseFlat) `sepBy` space
    where
      parseFlat   = parseVar <|> parseCom
      parseNested = parens parseInfixApp
      parens      = between (symbol "(") (symbol ")")

parseAppOp:: Parser (Expr String -> Expr String -> Expr String)
parseAppOp = symbol "@" >> return (:@)

parsePrefixApp :: Parser (Expr String)
parsePrefixApp = do
    app <- parseAppOp
    l   <- parsePrefixExpr
    r   <- parsePrefixExpr
    return (app l r)

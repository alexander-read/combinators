{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

----------------------------------------------------------------------------
-- |
-- Module      : Combinators.Pretty
-- Description : Pretty printer for Combinatory and Lambda Expressions
--
----------------------------------------------------------------------------
module Combinators.Pretty
    ( pprInf
    , pprLInf
    , pprNested
    ) where

import Prelude hiding ( (<>) )

import Combinators.Calculus
import Combinators.Lambda

import Text.PrettyPrint

----------------------------------------------------------------------------
---- * Pretty Printer

pprInf :: Expr String -> IO ()
pprInf = putStrLn . render . pprCombinator
  where
    pprCombinator expr = case expr of
      Var x  -> text x
      Com c  -> pprConstant c
      l :@ r -> parens $ pprCombinator l <> pprCombinator r

pprLInf :: LExpr String -> IO ()
pprLInf = putStrLn . render . pprLambda
  where
    pprLambda expr = case expr of
      LVar x  -> text x
      Lam x b -> text "λ" <> text x <> text "." <> pprLambda b
      App l r -> parens $ pprLambda l <> pprLambda r

pprNested :: String -> IO ()
pprNested = putStrLn . render . nest 4 . text

pprConstant :: Constant -> Doc
pprConstant = text . show
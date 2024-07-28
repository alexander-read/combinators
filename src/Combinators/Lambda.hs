{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE StrictData        #-}

----------------------------------------------------------------------------
-- |
-- Module      : Combinators.Lambda
-- Description : A lambda calculus to test combinator compilation
--
----------------------------------------------------------------------------
module Combinators.Lambda
  ( LExpr (..)
  ) where

----------------------------------------------------------------------------
---- * Grammar
  
-- | A very simple grammar, update with de Bruijn indices (cf. Bird and Paterson, 1999)
data LExpr a = LVar a
             | Lam a (LExpr a)         -- Lambda abstraction
             | App (LExpr a) (LExpr a) -- Application
             deriving (Show, Eq)

instance Functor LExpr where
  fmap f expr = case expr of
    LVar a  -> LVar $ f a
    App l r -> App (fmap f l) (fmap f r)
    Lam x b -> Lam (f x) $ fmap f b


{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

----------------------------------------------------------------------------
-- |
-- Module      : Combinators.Calculus
-- Description : Grammar for Combinatory Logic
--
-- This module implements a grammar for a combinatory calculus along with
-- a 'small step' semantics including weak contraction, weak normalisation,
-- and weak head normalisation. The normalisation strategies implement normal
-- order reduction, giving us a call-by-need semantics
--
----------------------------------------------------------------------------
module Combinators.Calculus
  ( Expr (..)
  , Constant (..)
  , NotRedex (..)
  , hasFreeVar
  , hasVar
  , getVars
  , contract
  , whc
  , whNorm
  , norm
  ) where

import Combinators.MyPrelude

import qualified Data.Set as Set
import Control.Monad ( ap )
import Data.Either (fromRight)

----------------------------------------------------------------------------
---- * Grammar

data Expr a = Var a | Com Constant | !(Expr a) :@ !(Expr a)
  deriving (Show, Eq, Functor, Foldable, Traversable)
infixl 4 :@

-- | The atomic constants form our combinatory base
data Constant = S | K | I | B | C | W | S' | B' | C'
  deriving (Eq, Ord, Show)

instance Applicative Expr where
    pure  = Var
    (<*>) = ap

instance Monad Expr where
    expr >>= f = case expr of
        Var a  -> f a
        l :@ r -> (l >>= f) :@ (r >>= f)
        Com c  -> Com c

----------------------------------------------------------------------------
---- * Small Step Semantics

newtype NotRedex a = NotRedex a deriving (Show, Eq)

-- | Contract an occurrence of a weak redex to its contractum
contract :: Expr a -> Either (NotRedex (Expr a)) (Expr a)
contract expr = case expr of
  Com S  :@ x :@ y :@ z      -> Right $ x :@ z :@ (y :@ z)
  Com K  :@ x :@ _           -> Right x
  Com I  :@ x                -> Right x
  Com B  :@ x :@ y :@ z      -> Right $ x :@ (y :@ z)
  Com C  :@ x :@ y :@ z      -> Right $ x :@ z :@ y
  Com W  :@ x :@ y           -> Right $ x :@ y :@ y
  Com S' :@ x :@ y :@ z :@ w -> Right $ x :@ (y :@ w) :@ (z :@ w)
  Com B' :@ x :@ y :@ z :@ w -> Right $ x :@ (y :@ (z :@ w))
  Com C' :@ x :@ y :@ z :@ w -> Right $ x :@ (y :@ w) :@ z
  e                          -> Left  $ NotRedex e

-- | Contract a weak head redex if there is one
whc :: Expr String -> Expr String
whc expr = case expr of
  (u :@ v) -> fromRight (whc u :@ v ) $ contract (u :@ v)
  u        -> u

-- | Weak head normalisation contracts weak head redexes as much as possible
-- to get an expression that is in weak head normal form
whNorm :: Expr a -> Expr a
whNorm expr = case expr of
  (u :@ v) -> let e = whNorm u :@ v in fromRight e (whNorm <$> contract e)
  t        -> t
    
-- | Normalise an expression
norm :: Expr a -> Expr a
norm expr = case whNorm expr of
  (u :@ v) -> norm u :@ norm v
  u        -> u

----------------------------------------------------------------------------
---- * Utility Functions

hasFreeVar :: Expr a -> Bool
hasFreeVar = foldAny $ const True

hasVar :: Eq a => a -> Expr a -> Bool
hasVar v = foldAny (== v)

getVars :: Ord a => Expr a -> Set.Set a
getVars expr = Set.fromList $ worker expr []
  where
    worker (Var a) ys  = a : ys
    worker (Com _) ys  = ys
    worker (l :@ r) ys = (worker l (worker r ys))

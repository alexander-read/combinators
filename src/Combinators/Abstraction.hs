{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

----------------------------------------------------------------------------
-- |
-- Module      : Combinators.Abstraction
-- Description : Compile lambda expressions using bracket abstraction
-- 
-- References are to:
-- * Curry, H.B. and Feys, R. (1958), Combinatory Logic Volume I
-- * Kogge, M. (1991), The Architecture of Symbolic Computers
-- * Peyton Jones, L. (1987), The Implementation of Functional Programming Languages
--
----------------------------------------------------------------------------
module Combinators.Abstraction
  ( compile
  , bracket
  ) where

import Combinators.Calculus
import Combinators.Lambda

----------------------------------------------------------------------------
---- * Compilation

-- | Translate any lambda expression into a combinatory expression
compile :: Eq a => LExpr a -> Expr a
compile expr = case expr of
    LVar x     -> Var x
    Lam x body -> bracket x $ compile body
    App e1 e2  -> compile e1 :@ compile e2

-- | Univariate bracket abstraction. This is essentially the algorithm `(fab)` given
-- by Curry and Feys (1958: §6A) wrapped with an optimisation function defined over an
-- extended basis, i.e., one containing combinators other than S, K, and I
bracket :: Eq a => a -> Expr a -> Expr a
bracket x m = case m of
    (u :@ v)         -> opt $ Com S :@ bracket x u :@ bracket x v
    (Var y) | y == x -> Com I
    u                -> Com K :@ u

-- | Optimisations for bracket abstraction (cf. Peyton Jones, 1987)
opt :: Expr a -> Expr a
opt expr = case expr of
    Com S :@ (Com K :@ u) :@ (Com K :@ v)      -> Com K  :@ (u :@ v)    -- K-optimisation
    Com S :@ (Com K :@ u) :@ (Com I)           -> u                     -- eta-contraction
    Com S :@ (Com K :@ u) :@ (Com B :@ v :@ w) -> Com B' :@ u :@ v :@ w -- B' combinator
    Com S :@ (Com K :@ u) :@ v                 -> Com B  :@ u :@ v      -- B combinator
    Com S :@ (Com B :@ u :@ v) :@ (Com K :@ w) -> Com C' :@ u :@ v :@ w -- C' combinator
    Com S :@ u :@ (Com K :@ v)                 -> Com C  :@ u :@ v      -- C combinator
    Com S :@ (Com B :@ u :@ v) :@ w            -> Com S' :@ u :@ v :@ w -- S' combinator
    Com S :@ u :@ Com I :@ v                   -> Com W  :@ u :@ v      -- W combinator
    Com B :@ u :@ Com I :@ v                   -> u :@ v                -- Kogge (1991: 309)
    _                                          -> expr
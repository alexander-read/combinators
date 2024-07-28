module Combinators.MyPrelude
  ( module Prelude
  , foldSum
  , foldProduct
  , foldAnd
  , foldOr
  , foldAll
  , foldAny
  )
  where

newtype Add = Add { getAdd :: Int  } deriving (Eq, Show)
newtype Mul = Mul { getMul :: Int  } deriving (Eq, Show)

newtype And = And { getAnd :: Bool } deriving (Eq, Show)
newtype Or  = Or  { getOr  :: Bool } deriving (Eq, Show)

instance Semigroup Mul where
    n <> m = Mul $ (getMul n) * (getMul m)

instance Semigroup Add where
    n <> m = Add $ (getAdd n) + (getAdd m)

instance Semigroup And where
    p <> q = And $ (getAnd p) && (getAnd q)

instance Semigroup Or where
    p <> q = Or $ (getOr p) || (getOr q)

instance Monoid Mul where
    mempty = Mul 1

instance Monoid Add where
    mempty = Add 0

instance Monoid And where
    mempty = And True

instance Monoid Or where
    mempty = Or False

-- | Smullyan's blackbird combinator (cf. Amar Shah)
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.) -- \ f g x y . f (g x y)

foldSum :: Foldable t => t Int -> Int
foldSum = getAdd . foldMap Add

foldProduct :: Foldable t => t Int -> Int
foldProduct = getMul . foldMap Mul

foldAnd :: Foldable t => t Bool -> Bool
foldAnd = getAnd . foldMap And

foldOr :: Foldable t => t Bool -> Bool
foldOr = getOr . foldMap Or

foldAll :: Foldable t => (a -> Bool) -> t a -> Bool
foldAll p = getAnd ... foldMap $ And . p

foldAny :: Foldable t => (a -> Bool) -> t a -> Bool
foldAny p = getOr ... foldMap $ Or . p

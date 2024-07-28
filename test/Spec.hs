{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Combinators.Abstraction
import Combinators.Calculus
import Combinators.Lambda
import Combinators.Parser

import qualified Data.Set as Set
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "hasFreeVar" $ do
        it "SKI has no free variables" $ do
            hasFreeVar testExpr1 `shouldBe` False
        it "Sxyz has free variables" $ do
            hasFreeVar testExpr2 `shouldBe` True

    describe "hasVar" $ do
        it "SKI does not have the variable 'x'" $ do
            hasVar "x" testExpr1 `shouldBe` False
        it "Sxyz has the variable 'x'" $ do
            hasVar "x" testExpr2 `shouldBe` True

    describe "getVars" $ do
        it "SKI has no variables" $ do
            getVars testExpr1 `shouldBe` Set.empty
        it "Sxyz has three unique variables" $ do
            getVars testExpr2 `shouldBe` Set.fromList ["x", "y", "z"]

    describe "weak contraction" $ do
        it "SKI weakly contracts to nothing" $ do
            contract testExpr1 `shouldBe` (Left $ NotRedex testExpr1)
        it "Sxyz weakly contracts to xz(yz)" $ do
            contract testExpr2 `shouldBe` (Right $ Var "x" :@ Var "z" :@ (Var "y" :@ Var "z"))

    describe "weak head contraction" $ do
        it "SKI weak head contracts to SKI" $ do
            whc testExpr1 `shouldBe`testExpr1
        it "SKK(K(SKKK)) weak head contracts to K(K(SKKK))(K(K(SKKK)))" $ do
            whc testExpr3 `shouldBe` testExpr4

    describe "weak head normalisation" $ do
        it "Sxyz weak head normalises to xz(yz)" $ do
            whNorm testExpr2 `shouldBe` Var "x" :@ Var "z" :@ (Var "y" :@ Var "z")
        it "SKK(K(SKKK)) weak head normalises to K(SKKK)" $ do
            whNorm testExpr3 `shouldBe` (Com K :@ (Com S :@ Com K :@ Com K :@ Com K))

    describe "normalisation" $ do
        it "SKK(K(SKKK)) normalises to KK" $ do
            norm testExpr3 `shouldBe` Com K :@ Com K

    -- Tests for bracket abstraction
    describe "compilation" $ do
        it "(\\x. x) compiles to I" $ do
            compile testExpr5 `shouldBe` Com I
        it "(\\x. xx) compiles to SII" $ do
            compile testExpr6 `shouldBe` Com S :@ Com I :@ Com I
        it "(\\x. + x) compiles to +" $ do
            compile testExpr7 `shouldBe`Var "+"
        it "(\\x. + x x) compiles to ((S+)I)" $ do
            compile testExpr8 `shouldBe` Com S :@ (Var "+") :@ Com I
        it "K-optimisation" $ do
            compile testExpr9 `shouldBe` Com K :@ (Var "+" :@ Var "1")
        it "optimise test" $ do
            compile testExpr10 `shouldBe` Var "x"
        it "Successor function is SB" $ do
            compile testExpr11 `shouldBe` Com S :@ Com B
        it "(\\x.x) should parse" $ do
            maybe (LVar "") id (parseLambda "\\x.x") `shouldBe` testExpr5
        it "(\\x.\\y.\\z.y (x y z)) should parse" $ do
            maybe (LVar "") id (parseLambda "\\x.\\y.\\z.y (x y z)") `shouldBe` testExpr11

parseLambda :: String -> Maybe (LExpr String)
parseLambda str = case runParser parseLExpr str of
    Right (e,_) -> Just e
    Left _      -> Nothing 

testExpr1 :: Expr String
testExpr1 = Com S :@ Com K :@ Com I

-- Sxyz
testExpr2 :: Expr String
testExpr2 = Com S :@ Var "x" :@ Var "y" :@ Var "z"

-- SKK(K(SKKK))
testExpr3 :: Expr String
testExpr3 = Com S :@ Com K :@ Com K :@ (Com K :@ (Com S :@ Com K :@ Com K :@ Com K))

-- K(K(SKKK))(K(K(SKKK)))
testExpr4 :: Expr String
testExpr4 = Com K :@ (Com K :@ (Com S :@ Com K :@ Com K :@ Com K)) :@ (Com K :@ (Com K :@ (Com S :@ Com K :@ Com K :@ Com K)))

testExpr5 :: LExpr String
testExpr5 = Lam "x" (LVar "x")

testExpr6 :: LExpr String
testExpr6 = Lam "x" (App (LVar "x") (LVar "x"))

testExpr7 :: LExpr String
testExpr7 = Lam "x" (App (LVar "+") (LVar "x"))

testExpr8 :: LExpr String
testExpr8 = Lam "x" (App (App (LVar "+") (LVar "x")) (LVar "x"))

testExpr9 :: LExpr String
testExpr9 = Lam "x" (App (LVar "+") (LVar "1"))

testExpr10 :: LExpr String
testExpr10 = Lam "y" (App (App (Lam "z" (LVar "x")) (LVar "y")) (LVar "y"))

-- | Successor function (cf. Kogge: 310)
testExpr11 :: LExpr String
testExpr11 = Lam "x" (Lam "y" (Lam "z" (App (LVar "y") (App (App (LVar "x") (LVar "y")) (LVar "z")))))


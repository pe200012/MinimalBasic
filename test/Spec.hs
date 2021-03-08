{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           Lib
import           Data.Either                    ( fromRight, isLeft )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State.Lazy       ( StateT(runStateT)
                                                , evalStateT
                                                )
import           Data.HashMap.Lazy              ( empty )
import           Data.Text.Lazy                 ( pack, Text )

eval :: Expr -> Value
eval = fromRight undefined . runExcept . flip evalStateT empty . evalExpr

evalWithExcept :: Expr -> Either Text Value
evalWithExcept = runExcept . flip evalStateT empty . evalExpr

main :: IO ()
main = hspec $ do
    describe "evalExpr" $ do
        context "when evaluating constant" $ do
            it "should act like id on Int" $ do
                property $ \x -> eval (CON (I x)) == I x
            it "should act like id on Float" $ do
                property $ \x -> eval (CON (F x)) == F x
            it "should act like id on String" $ do
                property $ \(pack -> x) -> eval (CON (S x)) == S x
        context "when evaluating variable reference" $ do
            it "should throw error on non-existing variable" $ do
                evalWithExcept (VAR "A") `shouldSatisfy` isLeft
            it "should work after a variable binding" $ do
                evalWithExcept (LET "A" (CON (I 1)) (VAR "A")) `shouldBe` Right (I 1)
        context "when evaluating numeric expression" $ do
            it "should handle addition properly" $ do
                property $ \x y -> eval (ADD (CON (I x)) (CON (I y))) == I (x + y)
            it "should handle subtraction properly" $ do
                property $ \x y -> eval (SUB (CON (I x)) (CON (I y))) == I (x - y)
            it "should handle multiplication properly" $ do
                property $ \x y -> eval (MUL (CON (I x)) (CON (I y))) == I (x * y)
            it "should handle division properly" $ do
                property $ \x (Positive y) -> eval (DIV (CON (I x)) (CON (I y))) == I (x `div` y)
            it "should handle power properly" $ do
                property $ \x (NonNegative  y) -> eval (POW (CON (I x)) (CON (I y))) == I (x ^ y)


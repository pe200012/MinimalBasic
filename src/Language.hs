{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall#-}
module Language where

import           Arithmetic
import           Control.Arrow                  ( Arrow(second)
                                                , first
                                                )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.State.Lazy       ( MonadState(get, put)
                                                , StateT(runStateT)
                                                , modify
                                                )
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH       ( MakeBaseFunctor(makeBaseFunctor) )
import           Data.HashMap.Lazy              ( HashMap )
import           Data.Sequence                  ( Seq
                                                , empty
                                                , (|>)
                                                )
import           Data.Text.Lazy                 ( Text
                                                , pack
                                                )

type OutputT o = StateT (Seq o)

type MonadOutput o = MonadState (Seq o)

printScreen :: (MonadOutput o m) => o -> m ()
printScreen = modify . flip (|>)

clearScreen :: (MonadOutput o m) => m ()
clearScreen = put empty

data Statement = Arithmetic Expr
               | PrintScreen Expr
               | ClearScreen
               | Sequence [Statement]

makeBaseFunctor ''Statement

evalStatement :: (MonadState (HashMap Text Value, Seq Text) m, MonadError Text m) => Statement -> m Value
evalStatement = cata go
  where
    wrapFirst f = do
        (a, b) <- runStateT f . fst =<< get
        modify (first (const b))
        return a
    wrapSecond f = do
        (a, b) <- runStateT f . snd =<< get
        modify (second (const b))
        return a
    go :: (MonadState (HashMap Text Value, Seq Text) m, MonadError Text m) => StatementF (m Value) -> m Value
    go (SequenceF   xs) = foldl (>>) (return U) xs
    go (ArithmeticF x ) = wrapFirst (evalExpr x)
    go ClearScreenF     = wrapSecond clearScreen >> return U
    go (PrintScreenF x) = do
        x' <- wrapFirst (evalExpr x)
        case x' of
            S s -> wrapSecond (printScreen s) >> return U
            I x -> wrapSecond (printScreen (pack $ show x)) >> return U
            F x -> wrapSecond (printScreen (pack $ show x)) >> return U
            U   -> wrapSecond (printScreen "()") >> return U
            A x -> wrapSecond (printScreen (pack $ show x)) >> return U

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Lib where

import           Control.Monad.Except           ( MonadError(throwError)
                                                , liftEither
                                                , runExcept
                                                )
import           Control.Monad.State.Lazy       ( get )
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.Map.Lazy
import           Data.Text.Format               ( format )
import           Data.Text.Lazy                 ( Text )
import           Relude                  hiding ( Text
                                                , Type
                                                , empty
                                                , get
                                                )

data Expr
  = INT Int
  | STR Text
  | VAR Text
  | LET Text Expr Expr
  | NEG Expr
  | ADD Expr Expr
  | SUB Expr Expr
  | MUL Expr Expr
  | DIV Expr Expr
  | POW Expr Expr

data Func = SQR

data Type = TINTEGER | TSTRING deriving (Show, Eq)

data Value = I Int | S Text deriving (Show, Eq)

makeBaseFunctor ''Expr
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

{-
>>> typecheck (VAR "A")
Left "Variable not in scope: A"

>>> typecheck (LET "A" (STR "bbb") (VAR "A"))
Right TSTRING

>>> typecheck (ADD (INT 0) (INT 2))
Right TINTEGER

>>> typecheck (ADD (INT 0) (STR "a"))
Left "ADD expected NUMBER type but got TSTRING"

>>> typecheck (ADD (STR "b") (STR "a"))
Left "ADD expected NUMBER type but got TSTRING"

-}

typecheck :: Expr -> Either Text Type
typecheck = runExcept . flip evalStateT empty . cata go
  where
    checkNumeric a b op = do
        a' <- a
        b' <- b
        case (a', b') of
            (TINTEGER, TINTEGER) -> return TINTEGER
            (TINTEGER, _       ) -> throwError (format "{} expected NUMBER type but got {}" [op, show @Text b'])
            _                    -> throwError (format "{} expected NUMBER type but got {}" [op, show @Text a'])
    go :: (MonadState (HashMap Text Type) m, MonadError Text m) => ExprF (m Type) -> m Type
    go (INTF _          ) = return TINTEGER
    go (STRF _          ) = return TSTRING
    go (VARF name       ) = maybe (throwError (format "Variable not in scope: {}" [name])) return . lookup name =<< get
    go (LETF name t body) = t >>= modify . insert name >> body
    go (NEGF t          ) = do
        t' <- t
        case t' of
            TINTEGER -> return TINTEGER
            _        -> throwError (format "Cannot match NUMBER type with {}" [show @Text t'])
    go (ADDF a b) = checkNumeric a b "ADD"
    go (SUBF a b) = checkNumeric a b "SUB"
    go (MULF a b) = checkNumeric a b "MUL"
    go (DIVF a b) = checkNumeric a b "DIV"
    go (POWF a b) = checkNumeric a b "POW"
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arithmetic where

import           Control.Monad.Except           ( MonadError(throwError)
                                                , liftEither
                                                , runExcept
                                                )
import           Control.Monad.State.Lazy       ( MonadState
                                                , evalStateT
                                                , get
                                                , modify
                                                , runStateT
                                                )
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.HashMap.Lazy              ( HashMap
                                                , empty
                                                , insert
                                                , lookup
                                                )
import           Data.Text.Format               ( format )
import           Data.Text.Lazy                 ( Text )
import           Relude                  hiding ( Text
                                                , Type
                                                , empty
                                                , evalStateT
                                                , runStateT
                                                )
import           Test.QuickCheck
data Expr
  = VAR Text
  | CON Value
  | LET Text Expr Expr
  | NEG Expr
  | ADD Expr Expr
  | SUB Expr Expr
  | MUL Expr Expr
  | DIV Expr Expr
  | POW Expr Expr

data Func = SQR

data Type = TINTEGER | TSTRING | TFLOAT deriving (Show, Eq)

data Value = I Int | S Text | F Float deriving (Show, Eq)

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
            (TFLOAT  , TFLOAT  ) -> return TFLOAT
            (TINTEGER, TINTEGER) -> return TINTEGER
            (TSTRING , _       ) -> throwError (format "{} expected NUMBER type but got {}" [op, show @Text b'])
            _                    -> throwError (format "{} expected NUMBER type but got {}" [op, show @Text a'])
    go :: (MonadState (HashMap Text Type) m, MonadError Text m) => ExprF (m Type) -> m Type
    go (CONF (I _)      ) = return TINTEGER
    go (CONF (S _)      ) = return TSTRING
    go (CONF (F _)      ) = return TFLOAT
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

evalExpr :: (MonadState (HashMap Text Value) m, MonadError Text m) => Expr -> m Value
evalExpr = (>>) . liftEither . typecheck <*> cata go
  where
    unaryOP x cont = do
        x' <- x
        case x' of
            I x -> return (I (cont x))
            _   -> error "Impossible to reach here"
    binaryOP x y cont = do
        x' <- x
        y' <- y
        case (x', y') of
            (I x, I y) -> return (I (cont x y))
            _          -> error "Impossible to reach here"
    go :: (MonadState (HashMap Text Value) m, MonadError Text m) => ExprF (m Value) -> m Value
    go (CONF (I x)            ) = return (I x)
    go (CONF (F x)            ) = return (F x)
    go (CONF (S x)            ) = return (S x)
    go (VARF name             ) = maybe (throwError (format "Variable not in scope: {}" [name])) return . lookup name =<< get
    go (LETF name binding body) = binding >>= modify . insert name >> body
    go (NEGF x                ) = unaryOP x negate
    go (ADDF a b              ) = binaryOP a b (+)
    go (SUBF a b              ) = binaryOP a b (-)
    go (MULF a b              ) = binaryOP a b (*)
    go (DIVF a b              ) = do
        a' <- a
        b' <- b
        case (a', b') of
            (I a, I b) -> return (I (a `div` b))
            (F a, F b) -> return (F (a / b))
    go (POWF a b              ) = binaryOP a b (^)

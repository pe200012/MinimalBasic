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

module Lib where

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

{-
>>> helper = runExcept . flip runStateT empty . evalExpr

>>> helper (INT 0)
Right (I 0,fromList [])

>>> helper (STR "A")
Right (S "A",fromList [])

>>> helper (LET "A" (INT 0) (VAR "A"))
Right (I 0,fromList [("A",I 0)])

>>> helper (NEG (INT 0))
Right (I 0,fromList [])

>>> helper (ADD (INT 0) (INT 1))
Right (I 1,fromList [])

>>> helper (SUB (INT 1) (INT 1))
Right (I 0,fromList [])

>>> helper (MUL (INT 2) (INT 8))
Right (I 16,fromList [])

>>> helper (DIV (INT 23) (INT 3))
Right (I 7,fromList [])

>>> helper (POW (INT 1) (INT 3))
Right (I 1,fromList [])

-}


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
  go (INTF x                ) = return (I x)
  go (STRF x                ) = return (S x)
  go (VARF name             ) = maybe (throwError (format "Variable not in scope: {}" [name])) return . lookup name =<< get
  go (LETF name binding body) = binding >>= modify . insert name >> body
  go (NEGF x                ) = unaryOP x negate
  go (ADDF a b              ) = binaryOP a b (+)
  go (SUBF a b              ) = binaryOP a b (-)
  go (MULF a b              ) = binaryOP a b (*)
  go (DIVF a b              ) = binaryOP a b div
  go (POWF a b              ) = binaryOP a b (^)

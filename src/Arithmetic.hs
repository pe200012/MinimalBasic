{-# LANGUAGE LambdaCase #-}
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
{-# OPTIONS_GHC -Wall#-}

module Arithmetic where

import           Control.Monad.Except           ( MonadError(throwError)
                                                , liftEither
                                                , runExcept
                                                )
import           Control.Monad.State.Lazy       ( evalStateT )
import           Data.Functor.Foldable          ( Recursive(cata) )
import           Data.Functor.Foldable.TH       ( MakeBaseFunctor(makeBaseFunctor) )
import           Data.HashMap.Lazy              ( empty
                                                , insert
                                                , lookup
                                                , lookupDefault
                                                )
import           Data.Text.Format               ( format )
import           Data.Text.Lazy                 ( Text )
import           Relude                  hiding ( Text
                                                , Type
                                                , evalStateT
                                                , empty
                                                )
import           Data.Vector                    ( Vector
                                                , (!)
                                                , (//)
                                                )

data Expr
  = VAR Text
  | ARR Expr Int
  | ASS Text Int Expr
  | CON Value
  | LET Text Expr Expr
  | NEG Expr
  | ADD Expr Expr
  | SUB Expr Expr
  | MUL Expr Expr
  | DIV Expr Expr
  | POW Expr Expr

data Func = SQR

data Type = TINTEGER | TSTRING | TFLOAT | TARRAY Int Type | TUNIT deriving (Show, Eq)

data Value = I Int | S Text | F Float | A (Vector Value) | U deriving (Show, Eq)

makeBaseFunctor ''Expr
makeBaseFunctor ''Value

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
    checkValue = cata $ \case
        (IF _) -> TINTEGER
        (FF _) -> TFLOAT
        (SF _) -> TSTRING
        (AF a) -> TARRAY (length a) (a ! 0)
        UF     -> TUNIT
    checkNumeric a b op = do
        a' <- a
        b' <- b
        case (a', b') of
            (TFLOAT  , TFLOAT  ) -> return TFLOAT
            (TINTEGER, TINTEGER) -> return TINTEGER
            (TSTRING , _       ) -> throwError (format "{} expected NUMBER type but got {}" [op, show @Text b'])
            _                    -> throwError (format "{} expected NUMBER type but got {}" [op, show @Text a'])
    go :: (MonadState (HashMap Text Type) m, MonadError Text m) => ExprF (m Type) -> m Type
    go (CONF (I _)) = return TINTEGER
    go (CONF (S _)) = return TSTRING
    go (CONF (F _)) = return TFLOAT
    go (CONF (A a)) = return (TARRAY (length a) (checkValue (a ! 0)))
    go (CONF U    ) = return TUNIT
    go (VARF name ) = maybe (throwError (format "Variable not in scope: {}" [name])) return . lookup name =<< get
    go (ARRF t i  ) = do
        t' <- t
        case t' of
            TARRAY b t | b > i     -> return t
                       | otherwise -> throwError (format "Access beyond array boundary: {}" [i])
            _ -> throwError (format "Expected Array type but got {}" [show @Text t'])
    go (ASSF name i b) = do
        b'  <- b
        cxt <- get
        case lookup name cxt of
            Just (TARRAY bb t) | bb > i && t == b' -> return TUNIT
                               | t /= b'           -> throwError (format "Expected {} type but got {}" (show @Text <$> [t, b']))
                               | otherwise         -> throwError (format "Access beyond array boundary: {}" [i])
            Just t -> throwError (format "Expected Array type but got {}" [show @Text t])
            _      -> throwError (format "Variable not in scope: {}" [name])
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
    unaryOP x (i, f) = do
        x' <- x
        case x' of
            I x -> return (I (i x))
            F x -> return (F (f x))
            _   -> error "Impossible to reach here"
    binaryOP x y (i, f) = do
        x' <- x
        y' <- y
        case (x', y') of
            (I x, I y) -> return (I (i x y))
            (F x, F y) -> return (F (f x y))
            _          -> error "Impossible to reach here"
    go :: (MonadState (HashMap Text Value) m, MonadError Text m) => ExprF (m Value) -> m Value
    go (CONF (I x)) = return (I x)
    go (CONF (F x)) = return (F x)
    go (CONF (S x)) = return (S x)
    go (CONF (A x)) = return (A x)
    go (CONF U    ) = return U
    go (ARRF a i  ) = do
        a' <- a
        case a' of
            A arr -> return (arr ! i)
            _     -> error "Impossible to reach here"
    go (ASSF n i b) = do
        cxt <- get
        b'  <- b
        case lookupDefault (error "Impossible to reach here") n cxt of
            A arr -> modify (insert n (A $ arr // [(i, b')])) >> return U
            _     -> error "Impossible to reach here"
    go (VARF name             ) = maybe (throwError (format "Variable not in scope: {}" [name])) return . lookup name =<< get
    go (LETF name binding body) = binding >>= modify . insert name >> body
    go (NEGF x                ) = unaryOP x (negate, negate)
    go (ADDF a b              ) = binaryOP a b ((+), (+))
    go (SUBF a b              ) = binaryOP a b ((-), (-))
    go (MULF a b              ) = binaryOP a b ((*), (*))
    go (DIVF a b              ) = binaryOP a b (div, (/))
    go (POWF a b              ) = binaryOP a b ((^), (**))

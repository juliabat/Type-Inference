module Syntax(Ident, Expr(..), Type(..), Constraints)  where

type Ident = String

data Expr = Num Int
        | Var Ident
        | Lambda Ident Expr
        |  Expr :@: Expr

data Type = TInt
        | TVar Int
        | Type :->: Type
    deriving Show

type Constraints = [(Type, Type)]

module GPML.Types.AST where

import GPML.Prelude

import Data.Functor.Classes

type Name = Text

data Expr = DefineVar Name Expr
          | DefineFun Name [Name] (NonEmpty Expr)
          | Lambda [Name] (NonEmpty Expr)
          | App Expr [Expr]
          | Var Name
          | IntLit Int
          | DoubleLit Double
          | SourceCode [SourceCodeSeg]
          deriving (Show, Eq)

data SourceCodeSeg = Quote Text 
                   | UnQuote (NonEmpty Expr) 
                   deriving (Show, Eq)


data Value = VString Text
           | VInt Int
           | VDouble Double
           | VList [Value]
           | VLambda [Name] (NonEmpty Expr)
           deriving (Show, Eq)


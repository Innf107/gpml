module GPML.Types.AST where

import GPML.Prelude

import Data.Functor.Classes

type Name = Text

data Expr = DefineVar Name Expr
          | DefineFun Name [Name] [Expr]
          | App Expr [Expr]
          | Var Name
          | SourceCode [SourceCodeSeg]
          deriving (Show, Eq)

data SourceCodeSeg = Quote Text 
                   | UnQuote Expr 
                   deriving (Show, Eq)


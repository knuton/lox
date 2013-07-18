module Lox.Types where

type Id = Char
type Op = Char
data Term = Const Int
          | Var Id
          | Fn Id [Term]
          | Operation Op Term Term
            deriving (Show)

data Fml = Atom Id
         | Not Fml
         | And Fml Fml
         | Or Fml Fml
         | OnlyIf Fml Fml
         | Iff Fml Fml
         | Xor Fml Fml
           deriving (Show)

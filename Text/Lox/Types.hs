module Text.Lox.Types where

type Id = Char
type Op = Char
data Term = Const Int
          | Var Id
          | Fn Id [Term]
          | Operation Op Term Term
            deriving (Show)

data Fml = Atom Id
         | Eq Term Term
         | Neq Term Term
         | Pred Id [Term]
         | Not Fml
         | Forall Id Fml
         | Exists Id Fml
         | And Fml Fml
         | Or Fml Fml
         | OnlyIf Fml Fml
         | Iff Fml Fml
         | Xor Fml Fml
         | Diamond Fml
         | Box Fml
           deriving (Show)

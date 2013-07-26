Representing Formulas (`Text.Lox.Types`)
----------------------------------------

To transform formulas from their plaintext representation into different
target formats, it is convenient (in practice essential) to have an
abstract representation of the formula instead of doing the translation
in place, since this allows parsing to be independent of the target
format. Those abstract representations of formulas and terms are defined
in this module.

> module Text.Lox.Types where

> type Id = Char
> type Op = Char
> data Term = Const Int
>           | Var Id
>           | Fn Id [Term]
>           | Operation Op Term Term
>             deriving (Show)

> data Fml = Atom Id
>          | Eq Term Term
>          | Neq Term Term
>          | Pred Id [Term]
>          | Not Fml
>          | Forall Id Fml
>          | Exists Id Fml
>          | And Fml Fml
>          | Or Fml Fml
>          | OnlyIf Fml Fml
>          | Iff Fml Fml
>          | Xor Fml Fml
>          | Diamond Fml
>          | Box Fml
>            deriving (Show)

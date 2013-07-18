module Lox.Reader where

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Text.ParserCombinators.Parsec
import Lox.Types
import Lox.Parsing

-- |
-- | TODO
-- |

-- - XOR: What would be a "natural" place in the binding order?
-- - Handling Unicode connectives

statement = endBy formula eof

-- |
-- | Terms
-- |

term :: GenParser Char st Term
term = try addoperation <|> try fnApp <|> var <?> "term"

-- | Atomic Terms

var :: GenParser Char st Term
var = Var <$> letter

fn :: GenParser Char st Char
fn = letter

fnApp :: GenParser Char st Term
fnApp = do
    fnId <- fn
    tms <- parens . spaced $ terms
    return $ Fn fnId tms

-- | Complex Terms

addoperator = Operation <$> spaced (oneOf "+-")
addoperation = chainl1 (try muloperation <|> try fnApp <|> var) addoperator

muloperator = Operation <$> spaced (oneOf "/*")
muloperation = try (chainl1 ((try . parens $ addoperation) <|> try fnApp <|> var) muloperator)
               <|> parens muloperation

terms :: GenParser Char st [Term]
terms = sepBy term separator

-- |
-- | Formulas
-- |

formula :: GenParser Char st Fml
formula = try biconditional <|> try negation <|> atom

-- | Atomic Formulas

atom = Atom <$> letter

literal = flipped literal <|> atom

negation = flipped formula

-- | Complex Formulas

-- Conjunction

wedge :: GenParser Char st (Fml -> Fml -> Fml)
wedge = symbols [ "&&", "&" ] *> return And

conjunction = try (chainl1 conjuncts wedge) <|> parens conjunction
  where conjuncts = try (weakBinding disjunction) <|> literal

-- Disjunction

vee :: GenParser Char st (Fml -> Fml -> Fml)
vee = symbols [ "||", "|" ] *> return Or

disjunction = chainl1 disjuncts vee
  where disjuncts = try (strongBinding conjunction) <|> literal

-- Implication

arrow :: GenParser Char st (Fml -> Fml -> Fml)
arrow = symbols [ "->", "=>" ] *> return OnlyIf

implication = optionalParens (binop (arrow *> return OnlyIf) operand formula)
  where operand = try (strongBinding disjunction) <|> literal

-- Biconditional

doublearrow :: GenParser Char st (Fml -> Fml -> Fml)
doublearrow = symbols [ "<->", "<=>" ] *> return Iff

biconditional = chainl1 equivalents doublearrow
  where equivalents = try (foldl1 (<|>) (map (try.strongBinding) [implication, disjunction])) <|> literal

-- | Parsing Formulas

flipped p = Not <$> (oneOf "~" *> p)

weakBinding p = flipped (parens p) <|> parens p
strongBinding p = flipped (parens p) <|> p

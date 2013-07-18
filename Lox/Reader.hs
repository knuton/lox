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
-- - Optional mode: Check formulas, e.g. disallow §x & x=x§

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
fnApp = application (Fn <$> fn) term

-- | Complex Terms

addoperator = Operation <$> spaced (oneOf "+-")
addoperation = chainl1 (try muloperation <|> try fnApp <|> var) addoperator

muloperator = Operation <$> spaced (oneOf "/*")
muloperation = try (chainl1 ((try . parens $ addoperation) <|> try fnApp <|> var) muloperator)
               <|> parens muloperation

-- |
-- | Formulas
-- |

formula :: GenParser Char st Fml
formula = try biconditional <|> try negation <|> atom

-- | Atomic Formulas

atom = tryAllOf [
         binop (symbol "=" *> return Eq) term term,
         binop (symbol "/=" *> return Neq) term term,
         application (Pred <$> letter) term,
         Atom <$> letter
       ]

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
  where equivalents = tryAllOf (map strongBinding [implication, disjunction]) <|> literal

-- | Parsing Formulas

flipped p = Not <$> (oneOf "~" *> p)

weakBinding p = try (flipped (parens p)) <|> parens p
strongBinding p = try (flipped (parens p)) <|> p

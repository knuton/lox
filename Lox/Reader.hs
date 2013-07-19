module Lox.Reader where

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Text.ParserCombinators.Parsec
import Lox.Types
import Lox.Parsing

-- |
-- | TODO
-- |

-- | LUXURY ITEMS
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
formula = try biconditional <|> try negation <|> try quantification <|> try modaloperation <|> atom

-- | Atomic Formulas

atom = tryAllOf [ binop (symbol "=" *> return Eq) term term
                , binop (symbol "/=" *> return Neq) term term
                , application (Pred <$> letter) term
                , Atom <$> letter
                ]

unary = try (flipped quantification) <|> try modaloperation <|> try (flipped unary) <|> atom

-- | Complex Formulas

-- Quantifiers

allquant = symbol "forall" *> return Forall
exquant = symbol "exists" *> return Exists

quantification = do
    quantifier <- allquant <|> exquant
    identifier <- letter
    spaced (char '.')
    fml <- try (parens formula) <|> unary
    return $ quantifier identifier fml

-- Negation

negation = flipped formula

-- Conjunction

wedge :: GenParser Char st (Fml -> Fml -> Fml)
wedge = symbols [ "&&", "&" ] *> return And

conjunction = try (chainl1 conjuncts wedge) <|> parens conjunction
  where conjuncts = try (weakBinding disjunction) <|> quantification <|> unary

-- Disjunction

vee :: GenParser Char st (Fml -> Fml -> Fml)
vee = symbols [ "||", "|" ] *> return Or

disjunction = chainl1 disjuncts vee
  where disjuncts = try (strongBinding conjunction) <|> unary

-- Implication

arrow :: GenParser Char st (Fml -> Fml -> Fml)
arrow = symbols [ "->", "=>" ] *> return OnlyIf

implication = optionalParens (binop (arrow *> return OnlyIf) operand formula)
  where operand = try (strongBinding disjunction) <|> unary

-- Biconditional

doublearrow :: GenParser Char st (Fml -> Fml -> Fml)
doublearrow = symbols [ "<->", "<=>" ] *> return Iff

biconditional = chainl1 equivalents doublearrow
  where equivalents = tryAllOf (map strongBinding [implication, disjunction]) <|> unary

-- Modal Operators

diamond, box :: GenParser Char st (Fml -> Fml)
diamond = symbol "<>" *> return Diamond
box = symbol "[]" *> return Box

modaloperation = do
    modop <- diamond <|> box
    fml <- try (parens formula) <|> unary
    return $ modop fml

-- | Parsing Formulas

flipped p = Not <$> (symbol "~" *> p)

weakBinding p = try (flipped (parens p)) <|> parens p
strongBinding p = try (flipped (parens p)) <|> p

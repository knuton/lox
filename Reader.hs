module Reader where

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Text.ParserCombinators.Parsec

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

--- Parsing
-- | Parses a space or tab.
spaceChar :: GenParser Char st Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: GenParser Char st Char
nonspaceChar = satisfy $ \x -> x /= '\t' && x /= '\n' && x /= ' ' && x /= '\r'

-- | Skips zero or more spaces or tabs.
skipSpaces :: GenParser Char st ()
skipSpaces = skipMany spaceChar
---

separator = spaced . char $ ','

spaced p = skipSpaces *> p <* skipSpaces
parens p = char '(' *> p <* char ')'

-- END Parsing Helpers

fn :: GenParser Char st Char
fn = letter

var :: GenParser Char st Term
var = Var <$> letter

fnApp :: GenParser Char st Term
fnApp = do
    fnId <- fn
    tms <- parens . spaced $ terms
    return $ Fn fnId tms

addoperator = Operation <$> spaced (oneOf "+-")
addoperation = chainl1 (try muloperation <|> try fnApp <|> var) addoperator

muloperator = Operation <$> spaced (oneOf "/*")
muloperation = try (chainl1 ((try . parens $ addoperation) <|> try fnApp <|> var) muloperator)
               <|> parens muloperation

term :: GenParser Char st Term
term = try addoperation <|> try muloperation <|> try fnApp <|> var <?> "term"

terms :: GenParser Char st [Term]
terms = sepBy term separator

statement = endBy formula eof

-- FMLs

atom = Atom <$> letter

flipped p = Not <$> (oneOf "~" *> p)

literal = flipped literal <|> atom

negation = flipped formula

vee :: GenParser Char st (Fml -> Fml -> Fml)
vee = spaced (char '|') *> return Or

disjunction = chainl1 disjuncts vee
  where
    disjuncts = try conjunction <|> (try . flipped.parens $ conjunction) <|> literal

wedge :: GenParser Char st (Fml -> Fml -> Fml)
wedge = spaced (char '&') *> return And

conjunction = try (chainl1 conjuncts wedge) <|> parens conjunction
  where
    conjuncts = (try . flipped.parens $ disjunction)
             <|> (try . parens $ disjunction)
             <|> literal

formula :: GenParser Char st Fml
formula = try disjunction <|> try conjunction <|> try negation <|> atom

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

-- END Parsing Helpers

fn :: GenParser Char st Char
fn = letter

var :: GenParser Char st Term
var = Var <$> letter

fnApp :: GenParser Char st Term
fnApp = do
    fnId <- fn
    char '('
    tms <- spaced terms
    char ')'
    return $ Fn fnId tms

addoperator = Operation <$> spaced (oneOf "+-")
addoperation = chainl1 (try muloperation <|> try fnApp <|> var) addoperator

muloperator = Operation <$> spaced (oneOf "/*")
muloperation = chainl1 (try fnApp <|> var) muloperator

term :: GenParser Char st Term
term = try addoperation <|> try muloperation <|> try fnApp <|> var <?> "term"

terms :: GenParser Char st [Term]
terms = sepBy term separator

statement = endBy term eof

-- FMLs

atom = Atom <$> letter

negation = Not <$> (oneOf "~" *> formula)

conjunction = do
    fml1 <- formula
    fml2 <- formula
    return $ And fml1 fml2

formula :: GenParser Char st Fml
formula = try negation <|> atom

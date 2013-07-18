module Lox.Parsing where

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (string)

-- |
-- | Whitespace
-- |

spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

skipSpaces = skipMany spaceChar

-- |
-- | Wrapping & Grouping
-- |

separator = spaced . char $ ','

spaced p = skipSpaces *> p <* skipSpaces

parens p = char '(' *> p <* char ')'
optionalParens p = p <|> parens p

tryAllOf ps = foldl1 (<|>) (map try ps)

-- |
-- | Operators
-- |

lexeme p = p <* skipSpaces

symbol name = skipSpaces *> lexeme (string name)

symbols names = tryAllOf (map symbol names)

application f arg = do
    fConstr <- f
    args <- parens . spaced $ sepBy arg separator
    return $ fConstr args

binop op left right = do
    l <- left
    o <- spaced op
    r <- right
    return $ o l r

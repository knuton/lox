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

-- |
-- | Operators
-- |

lexeme p = p <* skipSpaces

symbol name = skipSpaces *> lexeme (string name)

symbols names = foldl1 (<|>) (map (try.symbol) names)

binop op left right = do
    l <- left
    o <- spaced op
    r <- right
    return $ o l r

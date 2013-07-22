module Text.Lox.Readers.Document (constituents) where

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Text.ParserCombinators.Parsec
import Text.Lox.Parsing

constituents :: Char -> GenParser Char st [Either String String]
constituents sep = many1 (try (plain sep) <|> (loxStatement sep)) <* eof

plain :: Char -> GenParser Char st (Either String String)
plain sep = Left <$> (try plainTail <|> many1 (noneOf [sep]))
  where
    plainTail = char sep *> many1 (noneOf [sep]) <* eof

loxStatement :: Char -> GenParser Char st (Either String String)
loxStatement sep = Right <$> (char '§' *> many1 (noneOf [sep]) <* char '§')

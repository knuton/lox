module Text.Lox.Readers.Document (constituents) where

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Text.ParserCombinators.Parsec
import Text.Lox.Parsing

constituents :: Char -> GenParser Char st [Either String String]
constituents sep = many1 (try (code sep) <|> try (plain sep) <|> (loxStatement sep)) <* eof


plain :: Char -> GenParser Char st (Either String String)
plain sep = Left <$> (try plainTail <|> try (many1 (noneOf [sep])) <|> unmatched)
  where
    plainTail =
      (sep:) <$> (char sep *> many (noneOf [sep]) <* eof)
    unmatched =
      (\x -> (sep:x) ++ "\n") <$> (char sep *> many1 (noneOf [sep, '\n']) <* char '\n')

loxStatement :: Char -> GenParser Char st (Either String String)
loxStatement sep = Right <$> (char sep *> many1 (noneOf [sep, '\n']) <* char sep)

code :: Char -> GenParser Char st (Either String String)
code sep = Left <$> do
    initial <- many (noneOf [sep, '`'])
    char '`'
    statement <- many (noneOf "`")
    char '`'
    return $ initial ++ "`" ++ statement ++ "`"

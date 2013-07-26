Parsing Documents (`Text.Lox.Readers.Document`)
-----------------------------------------------

In order to preprocess documents with interspersed plaintext formulas, there
needs to be a way of deciding which parts are to be interpreted as formulas. The
exported function `constituents` gives a parser that divides the document in a
fitting way, when informed about which separator to use (by default this is
`§`).

Because it allows for convenient function application, the data structure used
to signify statementhood is `Either`. In this way, when applying a function to a
constituent, it will only really be applied if it is as \textrm{``right''}
constituent, that is, a statement.

> module Text.Lox.Readers.Document (constituents) where

> import Control.Applicative ((<$>), (<*>), (*>), (<*))
> import Text.ParserCombinators.Parsec
> import Text.Lox.Parsing

> constituents :: Char -> GenParser Char st [Either String String]
> constituents sep = many1 (try (code sep) <|> try (plain sep) <|> (loxStatement sep)) <* eof

> plain :: Char -> GenParser Char st (Either String String)
> plain sep = Left <$> (try plainTail <|> try (many1 (noneOf [sep])) <|> unmatched)
>   where
>     plainTail =
>       (sep:) <$> (char sep *> many (noneOf [sep]) <* eof)
>     unmatched =
>       (\x -> (sep:x) ++ "\n") <$> (char sep *> many1 (noneOf [sep, '\n']) <* char '\n')

> loxStatement :: Char -> GenParser Char st (Either String String)
> loxStatement sep = Right <$> (char sep *> many1 (noneOf [sep, '\n']) <* char sep)

Statements appearing within backticks are taken to be a mentioning of code
rather than use, and are therefore considered to be part of a \textrm{``left''}
constituent.

> code :: Char -> GenParser Char st (Either String String)
> code sep = Left <$> do
>     initial <- many (noneOf [sep, '`'])
>     char '`'
>     statement <- many (noneOf "`")
>     char '`'
>     return $ initial ++ "`" ++ statement ++ "`"

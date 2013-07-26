General Parsing (`Text.Lox.Parsing`)
------------------------------------

The combinatoric parser library Parsec is used for creating the parsers
that form an essential part of this piece of software. Combinatoric
parsers are quite different from more traditional parser generation
approaches like `lex` and `yacc`, in that parsers are built up of
primitives offered by the library which can be *combined* to yield more
complex parsers.

Parsec makes heavy use of monads to hide away the parser's state and so
in writing parsers there is usually little need to think about state.
However, to effectively work with monads and more generally applicative
functors, some good knowledge of these abstractions is necessary.

This module offers some reusable parsers that help enhance readability
by encapsulating frequently used patterns.

> module Text.Lox.Parsing where

> import Control.Applicative ((<$>), (<*>), (*>), (<*))
> import Text.ParserCombinators.Parsec
> import Text.Parsec.Char (string)

> -- |
> -- | Whitespace
> -- |

> spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

> skipSpaces = skipMany spaceChar

> -- |
> -- | Wrapping & Grouping
> -- |

> separator = spaced . char $ ','

> spaced p = skipSpaces *> p <* skipSpaces

> parens p = char '(' *> p <* char ')'
> optionalParens p = p <|> parens p

> tryAllOf ps = foldl1 (<|>) (map try ps)

> -- |
> -- | Operators
> -- |

> lexeme p = p <* skipSpaces

> symbol name = skipSpaces *> lexeme (string name)

> symbols names = tryAllOf (map symbol names)

> application f arg = do
>     fConstr <- f
>     args <- parens . spaced $ sepBy arg separator
>     return $ fConstr args

> binop op left right = do
>     l <- left
>     o <- spaced op
>     r <- right
>     return $ o l r

Library Module (`Text.Lox`)
---------------------------

The main module (not `Main` module) of Lox offers methods for
translating formulas from the input language to HTML or LaTeX, as well
as for processing formulas interspersed in larger documents.

The module structure is largely inspired by that of Pandoc.

> module Text.Lox
>             ( loxVersion
>             , formulaToHTML
>             , formulaToLaTeX
>             , preprocessDocument
>             , processFormulasBetween
>             ) where

> import Text.Lox.Reader
> import Text.Lox.Readers.Document
> import Text.Lox.Writers.Latex
> import Text.Lox.Writers.HTML
> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Error
> import Control.Applicative ((<$>))

> loxVersion :: String
> loxVersion = "0.0.1"

The exported functions `formulaToHTML` and `formulaToLaTeX` combine the
parsing capabilities of `Text.Lox.Reader` with translation functions
defined in `Text.Lox.Writers` modules.

> formulaToHTML :: String -> Either ParseError String
> formulaToHTML fml = asHTML <$> parse statement "" fml

> formulaToLaTeX :: String -> Either ParseError String
> formulaToLaTeX fml = asLatex <$> parse statement "" fml

This function treats the intended syntax for embedded logic formulas,
where formulas appear between paragraph characters. Typically the result
should be LaTeX math code which in turn can be translated to several
formats by Pandoc.

> preprocessDocument :: String -> String
> preprocessDocument doc = processFormulasBetween '§' transformer doc
>   where
>     transformer = \fml -> (\l -> "$" ++ l ++ "$") <$> formulaToLaTeX fml

In the spirit of Markdown, processing is very forgiving and will not
usually fail on error, but quietly pass and process as much of the
document as possible. If there is an error in the process of splitting a
document into \textrm{``left''} parts, which need no translation, and
\textrm{``right''} parts, which do, the original document will be
returned unchanged.

> processFormulasBetween :: Char
>                        -> (String -> Either ParseError String)
>                        -> String
>                        -> String
> processFormulasBetween separator transformer doc =
>     case parse (constituents separator) "" doc of
>       Left err -> doc
>       Right parts -> concat (map (processPart transformer) parts)

Similarly, if transforming one of the \textrm{``right''} parts fails,
the program won't exit but simply leave the part in question unchanged.
While this is beneficial for flexibility, it might be detrimental for
analysis if the user wants to understand or find errors. To make the
tool more useful an optional strict mode would hence be a good addition.

> processPart :: (String -> Either ParseError String)
>             -> Either String String
>             -> String
> processPart transformer part = case transformer <$> part of
>                      -- TODO strict-mode
>                      Left x -> x
>                      Right (Left err) -> if "mode" == "strict"
>                                          then error "err"
>                                          else "§" ++ (plain part) ++ "§"
>                      Right (Right x) -> x
>   where
>     plain (Right x) = x

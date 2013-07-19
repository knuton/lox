module Text.Lox (formulaToHTML, formulaToLatex) where
-- module Text.Lox (formulaToHTML, formulaToLatex, markdownToLatex, markdownToHTML) where

import Text.Lox.Reader
import Text.Lox.Writers.Latex
import Text.Lox.Writers.HTML
import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>))

formulaToHTML :: String -> Either ParseError String
formulaToHTML fml = asHTML <$> parse statement "" fml

formulaToLatex :: String -> Either ParseError String
formulaToLatex fml = asLatex <$> parse statement "" fml

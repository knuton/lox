module Text.Lox
            ( loxVersion
            , formulaToHTML
            , formulaToLaTeX
            , preprocessDocument
            , processFormulasBetween
            ) where

import Text.Lox.Reader
import Text.Lox.Readers.Document
import Text.Lox.Writers.Latex
import Text.Lox.Writers.HTML
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Control.Applicative ((<$>))

loxVersion :: String
loxVersion = "0.0.1"

formulaToHTML :: String -> Either ParseError String
formulaToHTML fml = asHTML <$> parse statement "" fml

formulaToLaTeX :: String -> Either ParseError String
formulaToLaTeX fml = asLatex <$> parse statement "" fml

preprocessDocument :: String -> String
preprocessDocument doc = processFormulasBetween '§' formulaToLaTeX doc

processFormulasBetween :: Char -> (String -> Either ParseError String) -> String -> String
processFormulasBetween separator transformer doc =
    case parse (constituents separator) "" doc of
      Left err -> doc
      Right parts -> concat (map (processPart transformer) parts)

processPart :: (String -> Either ParseError String) -> Either String String -> String
processPart transformer part = case transformer <$> part of
                     -- TODO strict-mode
                     Left x -> x
                     Right (Left err) -> if "mode" == "strict" then error "err" else "§" ++ (plain part) ++ "§"
                     Right (Right tex) -> "$" ++ tex ++ "$"
  where
    plain (Right x) = x

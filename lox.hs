module Main where

import Text.Lox
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Control.Applicative ((<$>))

main :: IO ()
main = do
    ascii <- getContents
    results <- transform (lines ascii) formulaToHTML
    mapM_ putStrLn results

transform :: [String] -> (String -> Either ParseError String) -> IO [String]
transform [] transformer = return []
transform (l:ls) transformer = do
    result <- case transformer l of
      Left msg -> do
        return "Compile error"
      Right latex -> do
        return latex
    rest <- transform ls transformer
    return $ result:rest

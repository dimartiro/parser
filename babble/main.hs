module Main where

import Tokens
import Grammar

import Data.Map

main :: IO ()
main = do
    s <- getLine
    print $ parseGrammar s
    main

-- Ejemplo gramatica
-- Exp : Exp ‘+’ Exp | Exp ‘*’ Exp | ‘(’ Exp ‘)’ | NUM ;

parseGrammar :: String -> BabbleGrammar
parseGrammar s =  parser (alexScanTokens s)

--parseGrammarFile :: String -> BabbleGrammar
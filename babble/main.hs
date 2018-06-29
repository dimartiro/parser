module Main where

import Tokens
import Grammar

import Data.Map

main :: IO ()
main = do
    s <- getLine
    print $ parseGrammar s
    --print $ parser (alexScanTokens s)
    --print $ alexScanTokens s
    main

-- Ejemplo gramatica
-- Exp : Exp ‘+’ Exp | Exp ‘*’ Exp | ‘(’ Exp ‘)’ | NUM ;

parseGrammar :: String -> BabbleGrammar
parseGrammar s =  parser (alexScanTokens s)

parseGrammarFile :: String -> IO BabbleGrammar
parseGrammarFile path = do
    content <- readFile path
    return (parseGrammar content)
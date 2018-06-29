module Main where

import Tokens
import Grammar

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

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

parseGrammarFile path = do
    content <- readFile path
    return (parseGrammar content)

data GrammarTree = 
    GrammarTree {
        symbol :: Symbol,
        successors :: [GrammarTree]
        }

generateGrammarTree bgm = generateTreeNodes (NonTerminal (initial bgm)) bgm

generateTreeNodes :: Symbol -> BabbleGrammar -> GrammarTree
generateTreeNodes (Terminal symbol) bgm = GrammarTree (Terminal symbol) [] 
generateTreeNodes (NonTerminal symbol) bgm = GrammarTree (NonTerminal symbol) [generateTreeNodes sym bgm | sym <- symbol_list (pickRandomProduction ((grammar bgm) Map.! symbol))]
    
pickRandomProduction :: [Production] -> Production
pickRandomProduction productions = productions !! 0

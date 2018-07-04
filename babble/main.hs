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
        deriving(Show, Eq);

data Restriction = 
    Unrestricted |
    Max Int |
    Min Int 
    deriving(Show, Eq);


generateGrammarTree :: BabbleGrammar -> Restriction -> GrammarTree
generateGrammarTree bgm restriction = generateTreeNodes (NonTerminal (initial bgm)) bgm restriction 0

generateTreeNodes :: Symbol -> BabbleGrammar -> Restriction -> Int -> GrammarTree
generateTreeNodes (Terminal symbol) bgm restriction height = GrammarTree (Terminal symbol) [] 
generateTreeNodes (NonTerminal symbol) bgm restriction height = GrammarTree (NonTerminal symbol) [generateTreeNodes sym bgm restriction (height+1) | sym <- symbol_list (pickRandomProduction (possibleNodes bgm symbol restriction height))]
    
possibleNodes :: BabbleGrammar -> String -> Restriction -> Int -> [Production]
-- Try to return only terminals if height gte max
possibleNodes bgm symbol (Max max) height
    | (height >= max) && (not (null terminal_list)) = terminal_list
    | otherwise = (grammar bgm) Map.! symbol
    where terminal_list = filter isAllTerminal ((grammar bgm) Map.! symbol)
          isAllTerminal production = all isTerminal (symbol_list production) 
          isTerminal (Terminal _) = True
          isTerminal _ = False

-- Try to return only non terminals if height lt min
possibleNodes bgm symbol (Min min) height
    | (height < min) && (not (null non_terminal_list)) = non_terminal_list
    | otherwise = (grammar bgm) Map.! symbol
    where non_terminal_list = filter isAnyNonTerminal ((grammar bgm) Map.! symbol)
          isAnyNonTerminal production = any isNonTerminal (symbol_list production) 
          isNonTerminal (NonTerminal _) = True
          isNonTerminal _ = False

-- Return everything
possibleNodes bgm symbol _ _ = ((grammar bgm) Map.! symbol)

pickRandomProduction :: [Production] -> Production
pickRandomProduction productions = productions !! 0

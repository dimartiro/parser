module Main where

import Tokens
import Grammar

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.List

import System.Random (newStdGen, randomRs)
import Data.Foldable (for_)


main :: IO ()
main = do
    s <- getLine
    print $ parseGrammar s
    --print $ parser (alexScanTokens s)
    --print $ alexScanTokens s
    main

-- Ejemplo gramatica
-- Exp : Exp ‘+’ Exp | Exp ‘*’ Exp | ‘(’ Exp ‘)’ | NUM ;

data GrammarNode = 
    GrammarParentNode {
        symbol :: Symbol
    } |
    GrammarChildNode {
        symbol :: Symbol,
        parent :: GrammarNode
    }
    deriving(Eq);

instance Show GrammarNode where
    show (GrammarParentNode (NonTerminal t)) = t
    show (GrammarParentNode (Terminal t)) = t
    show (GrammarChildNode (NonTerminal t) _) = t
    show (GrammarChildNode (Terminal t) _) = t

data Restriction = 
    Unrestricted |
    Max Int |
    Min Int |
    Redefinition (Map.Map String [Double])
    deriving(Show, Eq);
    
parseGrammar s =  parser (alexScanTokens s)

parseGrammarFile path = do
    content <- readFile path
    return (parseGrammar content)

unparseGrammar (BabbleGrammar grammar initial ignorables) = foldr (\e1 e2 -> (unparseGrammarElement (fst e1) (snd e1)) ++ " ; " ++ e2) "" (Map.toList grammar)

unparseGrammarElement :: String -> [Production] -> String
unparseGrammarElement initial productions = initial ++ ":" ++ (intercalate " | " (map unparseProduction productions))

unparseProduction (Production symbols prob) = (intercalate " " (map unparseSymbol symbols)) ++ " %prob " ++ (show prob)
unparseSymbol (NonTerminal s) = s
unparseSymbol (Terminal s) = s

-- Generate GrammarNode tree
generateGrammarUp :: BabbleGrammar -> Restriction -> [Double] -> ([GrammarNode], [Double])
generateGrammarUp bgm restriction nums = generateNodeUp bgm [GrammarParentNode (NonTerminal (initial bgm))] restriction 0 nums

generateNodeUp :: BabbleGrammar -> [GrammarNode] -> Restriction -> Int -> [Double] -> ([GrammarNode], [Double])
generateNodeUp bgm gnodes restriction height numbers
    | isTerminalTree = (gnodes, numbers)
    | otherwise = generateNodeUp bgm (foldl (\x y -> x ++ y) [] nextNodes) restriction (height+1) nextNumbers
    where isTerminalTree = all isTerminal [symbol node | node <- gnodes]
          nextNodes = map (\(node, number) -> getNextNodes bgm node number restriction height) (zip gnodes numbers)
          nextNumbers = drop (length gnodes) numbers

getNextNodes :: BabbleGrammar -> GrammarNode -> Double -> Restriction -> Int -> [GrammarNode] 
getNextNodes bgm node number restriction height = [GrammarChildNode sym node | sym <- symbol_list (pickRandomProduction number (possibleNodes bgm (symbol node) restriction height))]

isTerminalNode :: GrammarNode -> Bool
isTerminalNode (GrammarChildNode s _) = isTerminal s
isTerminalNode (GrammarParentNode s) = isTerminal s

isTerminal :: Symbol -> Bool
isTerminal (Terminal _) = True
isTerminal _ = False

-- Possible Nodes selection ------------------------------------------------------
possibleNodes :: BabbleGrammar -> Symbol -> Restriction -> Int -> [Production]
-- Try to return only terminals if height gte max
possibleNodes bgm (NonTerminal symbol) (Max max) height
    | (height >= max) && (not (null terminal_list)) = terminal_list
    | otherwise = (grammar bgm) Map.! symbol
    where terminal_list = filter isAllTerminal ((grammar bgm) Map.! symbol)
          isAllTerminal production = all isTerminal (symbol_list production) 

-- Try to return only non terminals if height lt min
possibleNodes bgm (NonTerminal symbol) (Min min) height
    | (height < min) && (not (null non_terminal_list)) = non_terminal_list
    | otherwise = (grammar bgm) Map.! symbol
    where non_terminal_list = filter isAnyNonTerminal ((grammar bgm) Map.! symbol)
          isAnyNonTerminal production = any isNonTerminal (symbol_list production) 
          isNonTerminal (NonTerminal _) = True
          isNonTerminal _ = False

-- Return everything
possibleNodes bgm (NonTerminal symbol) _ _ = ((grammar bgm) Map.! symbol)

-- Just in case
possibleNodes bgm sym _ _ = [Production [sym] 1]
-----------------------------------------------------------------------------------

pickRandomProduction :: Double -> [Production] -> Production
pickRandomProduction rnum prod = pickProduction rnum (normalizeProductions prod)

pickProduction :: Double -> [Production] -> Production
pickProduction rnum (xs:prod)
    | rnum <= (prob xs) = xs
    | length prod > 0 = pickProduction (rnum - prob xs) prod
    | otherwise = xs

 
normalizeGrammar :: BabbleGrammar -> BabbleGrammar
normalizeGrammar bgm = BabbleGrammar (Map.fromList newGrammarProductions) (initial bgm) (ignorable bgm)
    where newGrammarProductions = [(key, (normalizeProductions productions)) | (key, productions) <- Map.toList (grammar bgm)]

normalizeProductions :: [Production] -> [Production]
normalizeProductions prod = [Production (symbol_list x) (prob x/sumPro prod)| x <- prod]

sumPro :: [Production] -> Double
sumPro prod = sum[ prob x| x <- prod]

getNewProductions :: String -> [Production] -> (Map.Map String [Double]) -> [Production]
getNewProductions key productions prob_map
    | hasKey = map applyNewProbability (zip productions ((prob_map Map.! key) ++ (repeat 0)))
    | otherwise = productions
    where applyNewProbability (original_production, new_prob) = (Production (symbol_list original_production) new_prob)
          hasKey = Map.member key prob_map

redefineGrammarProbabilities :: BabbleGrammar -> Restriction -> BabbleGrammar
redefineGrammarProbabilities bgm (Redefinition prob_map) = BabbleGrammar (Map.fromList newGrammarProductions) (initial bgm) (ignorable bgm)
    where newGrammarProductions = [(key, (getNewProductions key productions prob_map)) | (key, productions) <- (Map.toList (grammar bgm))]
redefineGrammarProbabilities bgm _ = bgm

-- generateValidStrings: Monad alert, beware
generateValidStrings bgm restriction = do
  gen <- newStdGen
  let randoms = randomRs (0 :: Double, 1 :: Double) gen
  let (a, _) = generateGrammarUp (redefineGrammarProbabilities bgm restriction) restriction randoms
  print $ (foldl (\x y -> x ++ (show y)) "" a)

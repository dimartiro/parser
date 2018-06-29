{
module Grammar where
import Tokens
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    's'     { TokenString $$ }
    'd'     { TokenDigit $$ }
    ':'     { TokenColon }
    ';'     { TokenSemiColon }
    '|'     { TokenPipe }
    'v'     { TokenValueInQuotes $$ }
    '_'     { TokenUnderscore }
    '%prob' { TokenProb }
    
%right in
%% 

Gram : 's' ':' ProductionLine ';'                { BabbleGrammar (Map.fromList [($1,$3)]) $1}
    | Gram 's' ':' ProductionLine ';'           { BabbleGrammar (Map.insert $2 (foldr (\x y -> x:y) $4 (Maybe.fromMaybe [] (Map.lookup $2 (grammar $1)))) (grammar $1)) (initial $1) }  

ProductionLine : Elem                                      { [ Production $1 1.0 ] }
               | Elem '%prob' 'd'                          { [ Production $1 $3 ] }
               | Elem '%prob' 'd' '|' ProductionLine        { ( Production $1 $3 ) : $5 }
               | Elem '|' ProductionLine      { ( Production $1 1.0 ) : $3 }

Elem : 'v'                                    { [ Terminal $1 ] }
     | 's'                                      { [ NonTerminal $1 ] }
     | 's' Elem                              { NonTerminal $1 :  $2 }
     | 'v' Elem                              { Terminal $1 :  $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Symbol =
    Terminal String 
    | NonTerminal String
    deriving (Eq, Show)

data Production =
   Production {
       symbol_list :: [Symbol],
       prob :: Double
   } 
   deriving (Eq, Show)

data BabbleGrammar =
   BabbleGrammar {
       grammar :: (Map.Map String [Production]),
       initial :: String
   }
   deriving (Eq, Show)
}
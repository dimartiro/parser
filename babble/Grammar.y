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
    '"'     { TokenQuote }
    '_'     { TokenUnderscore }
    '%prob' { TokenProb }
    
%right in
%% 

Gram : 's' ':' ProductionLine                 { BabbleGrammar (Map.fromList [($1,$3)]) $1}
    | Gram ';' 's' ':' ProductionLine         { BabbleGrammar (Map.insert $3 (foldr (\x y -> x:y) $5 (Maybe.fromMaybe [] (Map.lookup $3 (grammar $1)))) (grammar $1)) (initial $1) }  

ProductionLine : Elem                                      { [ Production $1 1.0 ] }
               | Elem '%prob' 'd'                          { [ Production $1 $3 ] }
               | ProductionLine '|' Elem '%prob' 'd'       { ( Production $3 $5 ) : $1 }
               | ProductionLine '|' Elem                   { ( Production $3 1.0 ) : $1 }

Elem : '"''s''"'                                    { [ Terminal $2 ] }
     | 's'                                      { [ NonTerminal $1 ] }
     | Elem 's'                                 { NonTerminal $2 :  $1 }
     | Elem '"''s''"'                               { Terminal $3 :  $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Symbol =
    Terminal String 
    | NonTerminal String
    deriving (Eq, Show)

data Production =
   Production [Symbol] Double
   deriving (Eq, Show)

data BabbleGrammar =
   BabbleGrammar {
       grammar :: (Map.Map String [Production]),
       initial :: String
   }
   deriving (Eq, Show)
}
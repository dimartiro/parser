{
module Tokens where
}

%wrapper "basic"

$digit = 0-9			-- digits

tokens :-

  $white+				;
  \:					                                            { \s -> TokenColon }
  \;					                                            { \s -> TokenSemiColon }
  \|					                                            { \s -> TokenPipe }
  \_					                                            { \s -> TokenUnderscore s }
  \"[^\:\;\|\"\_]+\"                              { \s -> TokenValueInQuotes (read s)}
  \%prob                                                  { \s -> TokenProb }
  (0|[1-9]$digit*)(\.$digit+)?([eE][\+\-]?($digit+)?)?	  { \s -> TokenDigit ((read s)::Double) }
  [^\:\;\|\"\_\ \t\r\n]+                                  { \s -> TokenString s }
{


-- The token type:        
data Token =
	TokenString String |
  TokenDigit Double |
  TokenColon |
  TokenSemiColon |
  TokenPipe |
  TokenUnderscore String |
  TokenValueInQuotes String |
  TokenProb  
	deriving (Eq,Show)
}

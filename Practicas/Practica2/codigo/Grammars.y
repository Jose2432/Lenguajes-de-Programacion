{
module Grammars where

import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      int             { TokenNum $$ }
      bool            { TokenBool $$ }
      op              { TokenOp $$ }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

/* DEFINE AQUÍ TUS GRAMÁTICAS PARA EL PARSER. */

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data ASA = Num Int
         | Boolean Bool
         | Op String [ASA]
          deriving(Show)

data Token = TokenNum Int
           | TokenBool Bool
           | TokenOp String
           | TokenPA
           | TokenPC
           deriving(Show)

lexer :: String -> [Token]
-- Aquí va tu código.

main = getContents >>= print . parse . lexer

}

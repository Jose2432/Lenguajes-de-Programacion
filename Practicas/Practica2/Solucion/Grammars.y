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
      '+'             { TokenSuma $$ }

%%

-- DEFINE AQUÍ TUS GRAMÁTICAS PARA EL PARSER. */

ASA : int                  { Num $1 }
    | bool                 { Boolean $1 }
    | '(' op LASA ')'      { Op $2 $3 }
    | '(' '+' LASA ')'  { Suma $2 $3}

LASA : ASA         { [$1] }
     | ASA LASA    { $1:$2 }

--bool : '#t'   { $1 }
--     | '#f'   { $1 }

--op : '+'    { $1 }
--   | '-'    { $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data ASA = Num Int
         | Boolean Bool
         | Op String [ASA]
         | Suma String [ASA]
          deriving(Show)

data Token = TokenNum Int
           | TokenBool Bool
           | TokenOp String
           | TokenPA
           | TokenPC
           | TokenSuma String
           deriving(Show)

lexer :: String -> [Token]
lexer [] = []
lexer (' ' : xs) = lexer xs
lexer ('(' : xs) = TokenPA:(lexer xs)
lexer (')' : xs) = TokenPC:(lexer xs)
--lexer ('+' : xs) = TokenSuma:(lexer xs)
lexer (x:xs)
    | isDigit x = lexNum (x:xs)

lexNum :: String -> [Token]
lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

main = getContents >>= print . parse . lexer

}

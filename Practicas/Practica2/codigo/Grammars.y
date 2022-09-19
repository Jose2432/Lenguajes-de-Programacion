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
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMult }
      '/'             { TokenDiv }
      'a'             { TokenAdd }
      's'             { TokenSub }

%%

-- DEFINE AQUÍ TUS GRAMÁTICAS PARA EL PARSER. */

ASA : int                  { Num $1 }
    | '(' '+' ASA ASA ')'  { Suma $3 $4 }
    | '(' '-' ASA ASA ')'  { Resta $3 $4 }
    | '(' '*' ASA ASA ')'  { Mult $3 $4 }
    | '(' '/' ASA ASA ')'  { Div $3 $4 }
    | '(' 'a' ASA ')'      { Add1 $3 }
    | '(' 's' ASA ')'      { Sub1 $3 }
    | op                   { Op $1 }
    | bool                 { Boolean $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data ASA = Num Int
         | Boolean Bool
         | Op String 
         | Suma ASA ASA
         | Resta ASA ASA
         | Mult ASA ASA
         | Div ASA ASA
         | Add1 ASA
         | Sub1 ASA
          deriving(Show)

data Token = TokenNum Int
           | TokenBool Bool
           | TokenOp String
           | TokenPA
           | TokenPC
           | TokenSuma
           | TokenResta
           | TokenMult
           | TokenDiv
           | TokenAdd
           | TokenSub
           deriving(Show)

lexer :: String -> [Token]
lexer [] = []
lexer (' ' : xs) = lexer xs
lexer ('(' : xs) = TokenPA:(lexer xs)
lexer (')' : xs) = TokenPC:(lexer xs)
lexer ('+' : xs) = TokenSuma:(lexer xs)
lexer ('-' : xs) = TokenResta:(lexer xs)
lexer ('*' : xs) = TokenMult:(lexer xs)
lexer ('/' : xs) = TokenDiv:(lexer xs)
lexer ('a' : xs) = TokenAdd:(lexer xs)
lexer ('s' : xs) = TokenSub:(lexer xs)
lexer (x:xs)
    | isDigit x = lexNum (x:xs)

lexNum :: String -> [Token]
lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

main = getContents >>= print . parse . lexer

}

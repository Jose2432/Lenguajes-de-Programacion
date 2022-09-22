module Interp where

import Grammars

data Value = NumV Int
           | BoolV Bool

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = show b

interp :: ASA -> Value
interp (Num(a)) = NumV(a)
interp (Boolean(a)) = BoolV(a)
interp (Op "+" [Num(a),Num(b)] ) = NumV(a+b)
interp (Op "-" [Num(a),Num(b)] ) = NumV(a-b)
interp (Op "*" [Num(a),Num(b)] ) = NumV(a*b)
interp (Op "/" [Num(a),Num(b)] ) =
  if b==0
    then error "Error. No puedes dividir entre cero(0)"
    else NumV(div a b)

interp (Op "add1" [Num(a)] ) = NumV(a+1)
interp (Op "sub1" [Num(a)] ) = NumV(a-1)
interp (Op ">" [Num(a),Num(b)] ) =
  if a>b
    then BoolV(True)
    else BoolV(False)

interp (Op "<" [Num(a),Num(b)] ) =
  if a<b
    then BoolV(True)
    else BoolV(False)

interp (Op "=" [Num(a),Num(b)] ) =
  if a==b
    then BoolV(True)
    else BoolV(False)

interp (Op "not" [Boolean(a)] ) =
  if a==False
    then BoolV(True)
    else BoolV(False)

interp (Op "or" [Boolean(a),Boolean(b)] ) =
  if a==False && b==False
    then BoolV(False)
    else BoolV(True)

interp (Op "and" [Boolean(a),Boolean(b)] ) =
  if a==True && b==True
    then BoolV(True)
    else BoolV(False)

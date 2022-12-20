module Interp where

import Grammars

data Value = NumV Int
           | BoolV Bool
<<<<<<< HEAD
<<<<<<< HEAD
           | OpV String
=======
--           | OpV String
>>>>>>> 3fe87e2720b6d68bdcde3f0cb8cee9a06339356e
=======
>>>>>>> 71027324cbd0635a74c259725893a92d0e9e821d

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = show b
<<<<<<< HEAD
<<<<<<< HEAD
    show (OpV x) = show x
=======
--    show (OpV x) = show x
>>>>>>> 3fe87e2720b6d68bdcde3f0cb8cee9a06339356e
=======
>>>>>>> 71027324cbd0635a74c259725893a92d0e9e821d

interp :: ASA -> Value
interp (Num(a)) = NumV(a)
interp (Boolean(a)) = BoolV(a)
<<<<<<< HEAD
<<<<<<< HEAD
interp (Op(a)) = OpV(a)
interp (Suma (Num(a)) (Num(b))) = NumV(a+b)
interp (Resta (Num(a)) (Num(b))) = NumV(a-b)
interp (Mult (Num(a)) (Num(b))) = NumV(a*b)
interp (Div (Num(a)) (Num(b))) = NumV(div a b)
interp (Add1 (Num(a))) = NumV(a+1)
interp (Sub1 (Num(a))) = NumV(a-1)
=======
--interp (Suma (Num(a)) (Num(b))) = NumV(a+b)
--interp (Op(a)) = OpV(a)
--interp (Suma [(Num(a)) (Num(b))]) = NumV(a+b)
>>>>>>> 3fe87e2720b6d68bdcde3f0cb8cee9a06339356e
=======
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
>>>>>>> 71027324cbd0635a74c259725893a92d0e9e821d

module Interp where

import Grammars

data Value = NumV Int
           | BoolV Bool
           | OpV String

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = show b
    show (OpV x) = show x

interp :: ASA -> Value
interp (Num(a)) = NumV(a)
interp (Boolean(a)) = BoolV(a)
interp (Op(a)) = OpV(a)
interp (Suma (Num(a)) (Num(b))) = NumV(a+b)
interp (Resta (Num(a)) (Num(b))) = NumV(a-b)
interp (Mult (Num(a)) (Num(b))) = NumV(a*b)
interp (Div (Num(a)) (Num(b))) = NumV(div a b)
interp (Add1 (Num(a))) = NumV(a+1)
interp (Sub1 (Num(a))) = NumV(a-1)

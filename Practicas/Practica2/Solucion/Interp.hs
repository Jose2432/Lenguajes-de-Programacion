module Interp where

import Grammars

data Value = NumV Int
           | BoolV Bool
--           | OpV String

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = show b
--    show (OpV x) = show x

interp :: ASA -> Value
interp (Num(a)) = NumV(a)
interp (Boolean(a)) = BoolV(a)
--interp (Suma (Num(a)) (Num(b))) = NumV(a+b)
--interp (Op(a)) = OpV(a)
--interp (Suma [(Num(a)) (Num(b))]) = NumV(a+b)

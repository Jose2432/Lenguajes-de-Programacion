module Interp where

import Grammars

data Value = NumV Int
           | BoolV Bool
<<<<<<< HEAD
           | OpV String
=======
--           | OpV String
>>>>>>> 3fe87e2720b6d68bdcde3f0cb8cee9a06339356e

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = show b
<<<<<<< HEAD
    show (OpV x) = show x
=======
--    show (OpV x) = show x
>>>>>>> 3fe87e2720b6d68bdcde3f0cb8cee9a06339356e

interp :: ASA -> Value
interp (Num(a)) = NumV(a)
interp (Boolean(a)) = BoolV(a)
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

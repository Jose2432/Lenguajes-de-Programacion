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
<<<<<<< HEAD:Practicas/Practica02/Solucion/Interp.hs
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
=======
    
interp :: ASA -> Value
interp (Num n) = NumV n
interp (Boolean b) = BoolV b
interp (Op s l) = aplica s (map interp l)
>>>>>>> a26a1149045498d6ff8f24f88cb5cc116a40650d:Practicas/Practica2/Solucion/Interp.hs

aplica :: String -> [Value] -> Value
aplica "+" l = NumV (foldr1 (+) (map extraeI l))
aplica "-" l = NumV (foldr1 (-) (map extraeI l))
aplica "*" l = NumV (foldr1 (*) (map extraeI l))
aplica "/" l = NumV (division (map extraeI l))
aplica "<" l = BoolV (multiparam "<" (map extraeI l))
aplica ">" l = BoolV (multiparam ">" (map extraeI l))
aplica "=" l = BoolV (multiparam "=" (map extraeI l))
aplica "and" l = BoolV (foldr1 (&&) (map extraeB l))
aplica "or" l = BoolV (foldr1 (||) (map extraeB l))
aplica "add1" [x] = NumV ((extraeI x) + 1)
aplica "sub1" [x] = NumV ((extraeI x) - 1)
aplica "not" [x] =  if (extraeB x) then (BoolV False) else (BoolV True)

multiparam :: String  -> [Int] -> Bool
multiparam _ [] = True
multiparam _ [x] = True
multiparam "<" (x:y:xs) = (x < y) && (multiparam "<" (y:xs))
multiparam ">" (x:y:xs) = (x > y) && (multiparam ">" (y:xs))
multiparam "=" (x:y:xs) = (x == y) && (multiparam "==" (y:xs))

division :: [Int] -> Int
division [x] = x
division (x:y:xs) = if y == 0 then error "No puedes dividir entre cero." else (division (c:xs))
    where c = div x y

extraeI :: Value -> Int
extraeI (NumV n) = n

<<<<<<< HEAD:Practicas/Practica02/Solucion/Interp.hs
interp (Op "or" [Boolean(a),Boolean(b)] ) =
  if a==False && b==False
    then BoolV(False)
    else BoolV(True)

interp (Op "and" [Boolean(a),Boolean(b)] ) =
  if a==True && b==True
    then BoolV(True)
    else BoolV(False)
>>>>>>> 71027324cbd0635a74c259725893a92d0e9e821d
=======
extraeB :: Value -> Bool
extraeB (BoolV b) = b
>>>>>>> a26a1149045498d6ff8f24f88cb5cc116a40650d:Practicas/Practica2/Solucion/Interp.hs

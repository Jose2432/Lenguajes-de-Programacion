module Interp where

import Grammars

data Value = NumV Int
           | BoolV Bool

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = show b

interp :: ASA -> Value
-- Aquí va tu código.


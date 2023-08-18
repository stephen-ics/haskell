module Syntax where

data Expr
    = Tr --true
    | Fl --false
    | Zero
    | isZero Expr 
    | Succ Expr --the "successor" expression
    | Pred Expr --the "predecessor" expression
    | If Expr Expr Expr
    deriving (Eq, Show)
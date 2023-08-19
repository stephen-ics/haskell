module Syntax where --the ast

data Expr
    = Tr --true
    | Fl --false
    | Zero
    | IsZero Expr 
    | Succ Expr --the "successor" expression
    | Pred Expr --the "predecessor" expression
    --Succ and Pred are built-in constructors provided by Haskell's Prelude, it represents the successor operation in Peano arithmetic
    | If Expr Expr Expr
    deriving (Eq, Show)
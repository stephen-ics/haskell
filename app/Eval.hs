module Eval where

import Syntax

import Data.Maybe

isNum Syntax.Zero = True --if the current value is Syntax.Zero, return true
isNum (Syntax.Succ t) = isNum t --this will recurse all the way down to isNum Syntax.Zero if it is a number e.g 2 is represented as Succ (Succ (Syntax.Zero))
isNum _ = False
--note: peano arithmetic only deal with natural numbers, negative numbers are not a part of this representation

isVal :: Expr -> Bool
isVal Syntax.Tr = True
isVal Syntax.Fl = False
isVal t | isNum t = True --'|' introduces a pattern guard, allowing you to include additional conditionals that must be satisfied for the pattern to match, in this case 't' must be a natural number
isVal _ = False
--normal form or 'nf' can not be further reduced, values are in normal form and consist of True, False, and literal numbers

eval' x = case x of --the ' is just to create a modified or related version of a function
    Syntax.IsZero Syntax.Zero -> Just Syntax.Tr
    Syntax.IsZero (Syntax.Succ t) | isNum t -> Just Syntax.Fl
    Syntax.IsZero t -> Syntax.IsZero <$> (eval' t) --consider the example 'IsZero (Pred (Succ Zero))'
    Syntax.Succ t -> Syntax.Succ <$> (eval' t)
    Syntax.Pred Syntax.Zero -> Just Syntax.Zero
    Syntax.Pred (Syntax.Succ t) | isNum t -> Just t
    Syntax.Pred t -> Syntax.Pred <$> (eval' t)
    --recursively loops through pred and succ to eventually get to a value
    Syntax.If Syntax.Tr c _ -> Just c --If True, return c (result of true branch)
    Syntax.If Syntax.Fl _ a -> Just a --If False, return a (result of false branch)
    Syntax.If t c a -> (\t' -> Syntax.If t' c a) <$> eval' t --For the general case where 't' is not a constant, 't' is evaluated as True or False and wrapped back into the If expression
    _ -> Nothing
--at the top level, we simply apply eval' until either a value or we're left with a value with no way to proceed, "stuck" defines a value in an undefined state

nf x = fromMaybe x (nf <$> eval' x)
--fromMaybe is a function from the Data.Maybe module, it takes a default value 'x' and a 'Maybe' value, if the Maybe value is Nothing it returns 'x' if the Maybe value is 'Just y' it returns 'y'

eval :: Expr -> Maybe Expr
eval t = case nf t of --evaluates 't', 'nft' is the result
    nft | isVal nft -> Just nft --If the result 'nft' is a value, it returns the value wrapped in the Just constructor
        | otherwise -> Nothing --term is "stuck", it returns Nothing


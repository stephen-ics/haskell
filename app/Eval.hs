module Eval where

import Syntax

isNum Syntax.Zero = True
isNum (Syntax.Succ t) = isNum t
isNum _ = False

isVal :: Expr -> Bool
isVal Syntax.Tr = True
isVal Syntax.Fl = False

nf x = fromMaybe x (nf <$> eval' x)
eval :: Expr -> Maybe Expr
eval t = case nf t of
    nft | isVal nft -> Just nft
        | otherwise -> Nothing --term is "stuck"

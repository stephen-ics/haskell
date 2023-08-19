module Parsec where

import Lexer
import Syntax

import Control.Monad.Identity
import qualified Text.Parsec.Expr as Ex

--prefix operators
table :: Ex.OperatorTable String () Identity Expr
table = [
    [ --precedence level 1
        Lexer.prefixOp "succ" Syntax.Succ, --associates the "succ" operator with the Succ function, passed into Lexer.prefixOp as an argument and returned as 'Ex.Operator String() Identity a' type
        Lexer.prefixOp "pred" Syntax.Pred,
        Lexer.prefixOp "isZero" Syntax.isZero
    ]
]

--if/then/else
ifthen :: Parser Expr
ifthen = do
    Lexer.reserved "if"
    cond  <- expr
    Lexer.reservedOp "then"
    tr <- expr --if the condition is true
    Lexer.reserved "else"
    fl <- expr --if the condition is false
    return (If cond tr fl)

--constants
true, false, zero :: Parser Expr
true = Lexer.reserved "true" >> return Syntax.Tr
false = Lexer.reserved "false" >> return Syntax.Fl
zero = reservedOp "0" >> return Syntax.Zero

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor --defines parser for expressions and factors using Text.Parsec.Expr.buildExpressionParser with precedence table and 'factor' to parse individual constants

factor :: Parser Expr
factor =
    true
    <|> false --parsing basic expressions with the function defined above
    <|> zero
    <|> ifthen
    <|> parens expr

contents :: Parser a -> Parser a --defines parser combinator that takes another parser, eats up the whitespace, parses value, expends end of input and returns parsed value
contents p = do
    Tok.whiteSpace Lexer.lexer
    r <- p
    eof
    return r

parseExpr s = parse (contents expr) "<stdin>" s
--parseExpr is a function that takes in one argument 's', the string to be parsed
--The parse function, provided by the Text.Parsec library takes in three arguments, the parser (contents expr), "<stdin>" which serve as the source name, used for error reporting to indicate the source of the input being parsed, while the last argument 's' is the string to be parsed
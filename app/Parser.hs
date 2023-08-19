module Parser where

import Syntax
import Tokens

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

--Lexer Section
lexer :: Tok.TokenParser () --lexer is the TokenParser created
lexer = Tok.makeTokenParser Tokens.langDef
--takes the langDef requirements and returns a TokenParser made by Tok.makeTokenParser

parens :: Parser a -> Parser a
parens = Tok.parens lexer --these functions take in lexer as an argument to specify the rules defined in lexer
--returns a new parser that is used to parse parenthesis while respecting the language definition settings

reserved :: String -> Parser ()--() defines a parser that does not need to capture any meaningful value, the only purpose is to indicate that the parser succeeded
reserved = Tok.reserved lexer
--takes a string argument and parses a reserved keyword, ensuring the parsed operator is not part of a longer identifier

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer
--parses sequences of values separated by semicolons

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
--takes a string argument and parses a reserved operator, ensuring the parsed operator is not part of a longer identifier

prefixOp :: String -> (a -> a) -> Ex.Operator String() Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)
--takes a parser for the operator symbol and a function that combines the operator and operand to produce an expression
--'>>' sequences two actions, reservedOp s verifies it is the correct operator and return constructs a new monadic action 'f' lifted to monadic context

--Parser Section
--prefix operators
table :: Ex.OperatorTable String () Identity Expr
table = 
    [
        [ --precedence level 1
            prefixOp "succ" Syntax.Succ, --associates the "succ" operator with the Succ function, passed into prefixOp as an argument and returned as 'Ex.Operator String() Identity a' type
            prefixOp "pred" Syntax.Pred,
            prefixOp "iszero" Syntax.IsZero
        ]
    ]

--if/then/else
ifthen :: Parser Expr
ifthen = do
    reserved "if"
    cond  <- expr
    reservedOp "then"
    tr <- expr --if the condition is true
    reserved "else"
    fl <- expr --if the condition is false
    return (If cond tr fl)

--constants
true, false, zero :: Parser Expr
true = reserved "true" >> return Syntax.Tr
false = reserved "false" >> return Syntax.Fl
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
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser [Expr]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s
--parseExpr is a function that takes in one argument 's', the string to be parsed
--The parse function, provided by the Text.Parsec library takes in three arguments, the parser (contents expr), "<stdin>" which serve as the source name, used for error reporting to indicate the source of the input being parsed, while the last argument 's' is the string to be parsed
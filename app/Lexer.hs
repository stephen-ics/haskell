module Lexer where

import Tokens
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

lexer :: Tok.TokenParser () --lexer is the TokenParser created
lexer = Tok.makeTokenParser Tokens.langDef
--takes the langDef requirements and returns a TokenParser made by Tok.makeTokenParser

parens :: Parser a -> Parser a
parens = Tok.parens lexer --these functions take in lexer as an argument to specify the rules defined in lexer
--returns a new parser that is used to parse parenthesis while respecting the language definition settings

reserved :: String -> Parser ()
reserved = Tok.reserved lexer
--takes a string argument and parses a reserved keyword, ensuring the parsed operator is not part of a longer identifier

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer
--parses sequences of values separated by semicolons

reservedOp :: String -> Parser ()
reservedOp = Tok.reversedOp lexer
--takes a string argument and parses a reserved operator, ensuring the parsed operator is not part of a longer identifier

prefixOp :: String -> (a -> a) -> Ex.Operator String() Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)
--takes a parser for the operator symbol and a function that combines the operator and operand to produce an expression
--'>>' sequences two actions, reservedOp s verifies it is the correct operator and return constructs a new monadic action 'f' lifted to monadic context
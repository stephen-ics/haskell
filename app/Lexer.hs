module Lexer where

import Tokens
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser Tokens.langDef

parens :: Parser a -> Parser a
parens = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reversedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Oprator String() Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

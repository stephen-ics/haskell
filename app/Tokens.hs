module Tokens where

import Text.Parsec.Token

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
{   
    Tok.commentStart = "{-",
    Tok.commentEnd = "-}",
    Tok.commentLine = "--",
    Tok.nestedComments = True,
    Tok.identStart = letter,
    Tok.identLetter = alphaNum <|> oneOf "_'",
    Tok.opStart = oneOf ":!#$%&++./<==>?@\\^|-~",
    Tok.opLetter = oneOf ":!#$%&++./<==>?@\\^|-~",
    Tok.reservedNames = reservedNames,
    Tok.reservedOpNames = reservedOps,
    Tok.caseSensitive = True
}

--defining a record named 'langDef' (not built-in) with various settings to configure the lexer
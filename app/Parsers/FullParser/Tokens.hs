module Tokens where

import Text.Parsec
import qualified Text.Parsec.Token as Tok --qualified allows you change the reference name of the imported module

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
    {   Tok.commentStart = "{-",
        Tok.commentEnd = "-}",
        Tok.commentLine = "--",
        Tok.nestedComments = True,
        Tok.identStart = letter,
        Tok.identLetter = alphaNum <|> oneOf "_'",
        Tok.opStart = oneOf ":!#$%&++./<==>?@\\^|-~",
        Tok.opLetter = oneOf ":!#$%&++./<==>?@\\^|-~",
        Tok.reservedNames = [],
        Tok.reservedOpNames = [],
        Tok.caseSensitive = True
    }

--defining a record named 'langDef' (the Tok.makeTokenParser function recognizes these keywords)
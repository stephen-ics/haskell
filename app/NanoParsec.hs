{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] } --'a' is the value representing the value the parser is expected to produce (an AST Node for the parsed expression), 'String' is the remaining portion of the stream yet to be parsed
--for example, in the case of 'Parser Int', 'Int' is the 'a', and this 'Parser' type is designed to parse input strings and produce integer AST Nodes

runParser :: Parser a -> String -> a --runParser runs 
runParser m s =
    case parse m s of
        [(res, [])] -> res --res is just a placeholder name for the parsed value 'a', the important part is that the String (list of Chars) is empty (everything has been parsed)
        [(_, rs)] -> error "Parser did not consume entire stream."
        _ -> error "Parser error."

--the parser is advanced by extracting a single character from the parser stream and returning a tuple containing itself and the rest of the stream, the parser will then scrutinize the character and either transform it in some portion of the output or advance the stream and proceed (depending on the character received) 
item :: Parser Char
item = Parser $ \s ->
    case s of
        [] -> []
        (c:cs) -> [(c, cs)]

--a bind operation for our parser type will take one parse operation and compose it over the result of the second parse function
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a 
unit a = Parser (\s -> [a, s])

instance Functor Parser where
    fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
    return = unit
    (>>=) = bind
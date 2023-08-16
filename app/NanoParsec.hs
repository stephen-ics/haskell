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
item = Parser $ \s -> --this raises the tuple [(c, s)] or the empty list [] into the parser type
    case s of
        [] -> []
        (c:cs) -> [(c, cs)]

--a bind operation for our parser type will take one parse operation and compose it over the result of the second parse function
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s  --parse p s results in the arguments of the concatMap function, the 's' lambda argument is used in parse p s, rather than concatMap
--concatMap is used to map a function over a list and then concatenate the resultant list, in this case it concatenates a list of a list of tuples into a single list of tuples
--' represents a slightly modified or primed version of that variable or name

unit :: a -> Parser a 
unit a = Parser (\s -> [a, s])

instance Functor Parser where
    fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s]) --cs is a funtion call within the Parser type that inputs 's' as an argument and returns (a, b) where 'a' is the parsed value and 'b' is the remaining string
--the '|' here separates the generator (a, b) <- cs s, and the transformer (f a, b)

instance Applicative Parser where 
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])
--when you bind values cs1 s you can choose to extract either the function or the parsed value from the tuple (if there is a function), here cs1 is completely different from the cs that represents the rest of a string
--the function will take a parsed value and produce a new parser, this will allow for more complex parsers for example 'p1' that parses an integer and 'p2' that parses an operator can be combined to create an operator that parses an expression (ofc Parser f is applied to Parser a as this is an applicative functor so the two must be lifted in the same context)

instance Monad Parser where
    return = unit
    (>>=) = bind

instance MonadPlus Parser where --represents monads
    mzero = failure --halts reading the stream and returns the empty stream
    mplus = combine --applies two parser functions over the same stream

instance Alternative Parser where --represents applicative functors
    empty = mzero
    (<|>) = option --(<|>) defines the infix operator of function that combines two optional paths of parser logic, switching to the second path if the first fails with the zero value

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s) --'s' is the string to be parsed and 'parse p' and 'parse q' are two parser functions that parse different value types, the ++ operator concatenates the two results

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
    case parse p s of --in the case that parse p s produces an empty list (successfully parsed), call parse q s, else return the response (error message presumably)
        [] -> parse q s
        res -> res

--the Alternative typeclass definition are the 'many' and 'some' functions, the 'many' function repeatedly a single function argument until the function fails and yields the result, the some function behaves the same except it will fail itself if there is not a single match

-- | One or more as if f a raises an error, the program immediately halts
some :: f a -> f [a]
some v = some_v where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

--applying the cons operator to the 'v' (the result of f a) creates a list with the single element 'v', this list
--then the resultant list with a single value is applied to the many_v list, effectively appending 'v' to the many_v list
--it is important to note that in parsing, input is not always the same string, but rather the remainder of the string that needs to be parsed

-- | Zero or more as if f a raises an error pure [] is returned
many :: f a -> f [a]
many v = many_v where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

--this function checks whether the current characer 'c' in the stream matches a given predicate 'p' (is a digit, is a letter, a specific word, etc)
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item 'bind' \c -> --'c' is provided by item which takes in a string 's' and outputs a character 'c'
    if p c
    then unit c
    else (Parser (\cs -> []))

oneOf :: [Char] -> Parser Char --a predicate that checks if a character (Char input in satisfy) is present in the string 's'
oneOf s = satisfy (flip elem s)
--flip takes a binary function that returns a new function with the arguments reversed a -> b -> c will become b -> c -> a
--elem takes in a character and a list of characters, and checks if the character is in the list of characters
--it is necessary to flip the elem function as the first argument is usually the character, but as we do not know the character at this point, we can pass in the string 's' as the first argument and pass in the character 'c' as the second argument when satisfy is called

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p 'chainl1' op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p 'chainl1' op = do {a <- p; rest a}
    where rest a = (do
            f <- op
            b <- p
            rest (f a b))
        <|> return a

--chainl and chainl1 is a parser combininator that parses left-associative expressions involving an initial value parsed by p and binary operators parsed by op. It uses the rest function to build a left-recursive expression tree, and it handles the case when no more operators are found using the <|> alternative combinator to return the original initial value
--this is like parsing infix expressions from building an interpreter in GO!
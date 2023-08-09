# Haskell Basics

# Syntax
 - :: Is a type annotation used to specify the type of an expression
 - -> Is used to denote function types, the last -> denotes the return type of the function
 - The syntax will look like 
```haskell 
    identifier :: arg1 type -> arg2 type -> return type
```

 - : The cons operator is used to instruct a new list by adding an element in front of the list a "push" operator
 - The syntax looks like 
```haskell
    (1 : (2 : ( 3: [])))  = [1, 2, 3]
```

# Functions
 - Functions are first class citizens in Haskell that can be passed around as an argument and yielded as a return value
```haskell
    compose f g = \x -> f (g x), this function identified by "compose" takes in two arguments f and g which returns an unamed function aka a lambda function and returns the composite function f(g(x))
```

# Data types
 - "data" is a keyword used to define an algebraic data type (sum types and product types)
 - "|"" Define sum types, an algebraic data type that consists of multiple options of type constructors
 - Product types define multiple fields into the same type, like a struct 
 - Records are a special product type that generate a special set of functions known as selectors which extract values of a specific field from the Records
```haskell
    data Sum = A int | B Bool, the sum data type with two type constructors
    data Prod = Prod Int Bool, the prod data type with two types, Int and Bool, Prod is the name of the data type
```

- Sums and products can be combined
```haskell
    data T1 = A Int Int | B Bool Bool
```

# Values
## Definitions:
- Homogeneous: Consisting of 1 data type | Heterogenous -> Consiting of multiple data tyeps
- Inductively defined "x" type: Constructed by combining simpler cases and a "x" operator -> e.g Inductively defined sum type is defined with a sum operator like :

- A list is a homogenous inductively defined sum type of linked cells parametrized over the type of its values
```haskell
    (1 : (2 : ( 3: [])))  = [1, 2, 3]
```
- A tuple is a heterogeneous product type parametrized over the types of its two values. Tuples have a special value-level syntax
```haskell
    data myTuple a b = myTuple a b
    myTuple = (1, 2) -> a tuple is defined by paraenthesis and separated with commas
```

# Pattern Matching
- Pattern matching allows us to discriminate on the constructors of a datatype, mapping separate cases to separate code paths and binding variables for each of the fields of the datatype
```haskell
    data Maybe a = Nothing | Just a, this will return different values based on the data type of the argument which can be constructed as Nothing or Just, a sum type 
    
    maybe :: b -> (a -> b) -> Maybe a -> b, this defines the types of the arguments and return value
    maybe n f Nothing = n, when n is constructed with the Nothing constructor, it returns the default value 'n'
    maybe n f (Just a) = f a, when n is constructed with the Just constructor which takes in the argument 'a' and returns a function with 'a' as the argument f(a)
```
- Top level (code that is not defined outside any blocks/functions) matches can be written identically as case statements
```haskell
    maybe :: b -> (a -> b) -> Maybe a -> b
    maybe n f x = case x of, x is the parameter that determines the case
        Nothing -> n
        Just a -> f a
```
- _ Is a wildcards that can be placed for patterns where the resulting value is not used
```haskell
    const :: a -> b -> a
    const x _ = x, a wildcard is used to match the argument 'b' 
```
- A subexpression refers to a smaller or nested part of a larger expression
- @ Bounds the subexpression in the pattern to the variable scoped on the right hand side of the pattern match
```haskell
    f :: Maybe (Maybe a) -> Maybe a
    f (Just x @ (Just _)) = x, the _ is a wildcard for the argument, (Just _) is the whole subexpression that is captured by x
```
- List and tuples have a special pattern syntax
- (x:xs) is special syntax for a list that separates it into a head and a tail
```haskell
    length :: [a] -> Int
    length [] = 0
    length (x:xs) = 1 + (length xs), this function recursively checks if the tail is empty and if not it will take the tail and add 1 to the length until the tail is empty and the whole list has been calculated

    first :: (a, b) -> a, this function takes a tuple and returns the first element
    first (a, b) = a
```
- A predicate is a function that yields a boolean, they can guard patterns
- Guards only allow the execution of a branch if the corresponding predicate yields to True
```haskell
    filter :: (a -> Bool) -> [a] -> [a], type definition
    filter pred [] = []
    filter pred (x:xs)
        | pred x = x : filter pred xs, 
        | otherwise = filter pred xs
```
- This constructs a filter with pred as the function and an empty list
- If the input list is non-empty and can be deconstructed into an empty head, then apply the rest of 'filter' based on whether x is true or false. if x is true then 'x' is included in the filtered output list. If x is false it is excluded from the filtered output list
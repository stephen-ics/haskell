# Haskell Basics
- An annotation of the book 'Write You a Haskell' by Stephen Diehl

## Syntax
 - :: Is a type annotation used to specify the type of an expression
 - -> Is used to denote function types, the last -> denotes the return type of the function
 - The syntax will look like 
```haskell 
    identifier :: arg1 type -> arg2 type -> return type
```

 - : The cons operator is used to instruct a new list by adding an element in front of the list a 'push' operator
 - The syntax looks like 
```haskell
    (1 : (2 : ( 3: [])))  = [1, 2, 3]
```

## Functions
 - Functions are first class citizens in Haskell that can be passed around as an argument and yielded as a return value
```haskell
    compose f g = \x -> f (g x) --this function identified by 'compose' takes in two arguments f and g which returns an unamed function aka a lambda function and returns the composite function f(g(x))
```

## Data types
 - 'data' is a keyword used to define an algebraic data type (sum types and product types)
 - '|' Define sum types, an algebraic data type that consists of multiple options of type constructors
 - Product types define multiple fields into the same type, like a struct 
 - Records are a special product type that generate a special set of functions known as selectors which extract values of a specific field from the Records
```haskell
    data Sum = A int | B Bool --the sum data type with two type constructors
    data Prod = Prod Int Bool --the prod data type with two types, Int and Bool, Prod is the name of the data type
```

- Sums and products can be combined
```haskell
    data T1 = A Int Int | B Bool Bool
```

## Values
- Homogeneous: Consisting of 1 data type | Heterogenous -> Consiting of multiple data tyeps
- Inductively defined 'x' type: Constructed by combining simpler cases and a 'x' operator -> e.g Inductively defined sum type is defined with a sum operator like :

- A list is a homogenous inductively defined sum type of linked cells parametrized over the type of its values
```haskell
    (1 : (2 : ( 3: [])))  = [1, 2, 3]
```
- A tuple is a heterogeneous product type parametrized over the types of its two values. Tuples have a special value-level syntax
```haskell
    data myTuple a b = myTuple a b
    myTuple = (1, 2) -> a --tuple is defined by paraenthesis and separated with commas
```

## Pattern Matching
- Pattern matching allows us to discriminate on the constructors of a datatype, mapping separate cases to separate code paths and binding variables for each of the fields of the datatype
```haskell
    data Maybe a = Nothing | Just a, this will return different values based on the data type of the argument which can be constructed as Nothing or Just, a sum type 
    
    maybe :: b -> (a -> b) -> Maybe a -> b --this defines the types of the arguments and return value
    maybe n f Nothing = n --when n is constructed with the Nothing constructor, it returns the default value 'n'
    maybe n f (Just a) = f a --when n is constructed with the Just constructor which takes in the argument 'a' and returns a function with 'a' as the argument f(a)
```
- Top level (code that is not defined outside any blocks/functions) matches can be written identically as case statements
```haskell
    maybe :: b -> (a -> b) -> Maybe a -> b
    maybe n f x = case x of --x is the parameter that determines the case
        Nothing -> n
        Just a -> f a
```
- _ Is a wildcards that can be placed for patterns where the resulting value is not used, it can also be used to generalize unwanted arguments
```haskell
    const :: a -> b -> a
    const x _ = x --a wildcard is used to match the argument 'b' 
```
- const is a built-in function part of the standard library, it takes two arguments but returns the first one
- A subexpression refers to a smaller or nested part of a larger expression
- @ Bounds the subexpression in the pattern to the variable scoped on the right hand side of the pattern match
```haskell
    f :: Maybe (Maybe a) -> Maybe a
    f (Just x @ (Just _)) = x --the _ is a wildcard for the argument, (Just _) is the whole subexpression that is captured by x
```
- List and tuples have a special pattern syntax
- (x:xs) is special syntax for a list that separates it into a head and a tail
```haskell
    length :: [a] -> Int
    length [] = 0
    length (x:xs) = 1 + (length xs) --this function recursively checks if the tail is empty and if not it will take the tail and add 1 to the length until the tail is empty and the whole list has been calculated

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

## Recursion
- In Haskell, all iteration over data structures is performed by recursion
- Entering a function in Haskell does not create a new stack frame, the logic of the function is simply entered with the arguments on the stack and yields result to the register
- In the case where functions return an invocation of itself in tail position (the position in a function where a recurisve call is the last operation before the function returns a result), the resulting logic is compiled via a jump instruction rather than a call
```haskell
    sum :: [Int] -> [Int]
    sum ys = go ys 0
        where
            go (x:xs) i = go xs (i+x)
            go [] i = i
```
- This sum function takes in a list of integers and outputs a list of integers. 'ys' is the name of the integer list while 0 is the initial accumulator.
- It uses an auxiliary function (a helper function) defined in the where statement. If 'go' takes a parameter of an empty list it returns i, if the parameter can be broken down into a head and a tail, it recursively calls upon the sum function to calculuate the final sum. The types for the go function are not explicitly stated

- Functions can be defined to recurse mutually on each other
```haskell
    even 0 = True
    even n = odd (n-1)

    odd 0 = False
    odd n = even (n-1)
```
- Even and odd recursively call each other until 0 is returned, in which the whether it is even or odd will be returned

## Laziness
- Haskell is a "lazy language", this means that computations are not performed until they are actually needed
- A 'thunk' is a function that has yet to be computed, when you bound a value to a variable, you're not performing the evaluation yet, rather you're creating a thunk that represents that computation. This allows the lazy evaluation to avoid unecessary calculations
- A thunk that is currently being evaluated may induce the evaluation of other thunks in the process
- An expression in "weak head normal form (WHNF)" refers to a partially evaluated expression where the outermost constructor (data type) has been evaluated but the arguments to the constructor might still be thunks
- The runtime has the task of determining which thunks are to be evaluated by the order in which they are connected to the main function node, this is known as "graph reduction"
```haskell
    ones = 1 : ones --infinite stream of 1's

    numsFrom n = n : numsFrom (n+1) --infinite count from n

    squares = map (^2) (numsFrom 0) --infinite stream of integer squares

    take :: Int -> [a] -> [a] 
    take n _ | n <= 0 = [] --'<=' is a comparison operator not an arrow, _ is used as a placeholder for a value you do not intent to use, in this case anything that is not an integer list!
    take n [] = []
    take n (x:xs) = x : take (n-1) xs

    take 5 squares -- [0, 1, 4, 9, 16]
```
- Infinite streams are possible in Haskell due to laziness, when you define an infinite stream, Haskell does not generate the entire infinite list at once, but rather it generates values on-the-fly as needed
- The 'take' function is used to consume values from infinite streams without evaluating the entire stream, it works efficiently with laziness
- In the example of take 5 square, Haskell only generates 5 squares due to laziness

- In Haskell, a 'bottom' sometimes called a "diverging term" is used to describe a non-terminating or undefined value, often represented as '⊥', it's value has no normal form and does not produce a valid result. It does not successfully compute and yield any meaningful output
- Laziness is Haskell allow for bottoms to be "threaded around" without immediately causing an error or divergence. These values can be passed around through expressions until they are forced to be evaluated
```haskell
    bot = bot --The expression 'bot' does not diverge since the second argument is not used in the body of const
    const 42 bot

    error :: String -> a
    undefined :: a
```
- Error and undefined will be two bottom terms frequently referenced when writing the scaffolding (initial structure) for incomplete programs

## Higher-Kinded Types
- The "type of types" in Haskell (just as types classify values, kinds classify types) is the language of kinds. Kinds are either an arrow (k -> k') or a star (*)
- (k -> k') indicates that a kind takes one kind and produces another kind while '*' is the simplest kind and represents a type that doesn't take any arguments, it's a 'concrete' type
- Higher kinded types are types that take other types to produce new types, like a function argument type that takes an argument of a type to return a value of a type
- The kind of Int is *, while the kind of 'Maybe' is * -> *. Haskell supports higher-kinded types which are types that take other types and construct a new type
- A type constructor in Haskell always has a kind which terminates in a *
```haskell
    data T1 f a = T1 (f a) --T1 :: (* -> *) -> * -> *, the (* -> *) is a higher-kinded type as it takes in a type and returns a type
```
- The three special types (,), (->), [] have special type-level syntatic sugar
```haskell
    (,) Int Int = (Int, Int) --tuples
    (->) Int Int = Int -> Int --functions
    [] Int = [Int] --lists
```

## Typeclasses
- A typeclass defines a collection of functions which need to conform to the given interface
- An implementation of an interface is called an 'instance'
- Typeclasses are effectively syntatic sugar for records of functions and nested records (dictionaries) of functions parametrized over the instance type 
- These dictionaries are implicitly threaded throughout the program when an overloaded identifier is used
- When a typeclass is used over a concrete type, the implementation is simply spliced (the call is replaced with the corresponding function) in at the call site
- When a typeclass is used over a polymorphic type (polymorphic types are types that can represent values of different specific types), an implicit dictionary parameter is added to the function so that the implemetation of the necessary functionality is passed with the polymorphic value
- Typeclasses are "open" and additional instances can always be added, but the defining feature of a typeclass is that the instance search always converges to a single type (when you have multiple instances of a typeclass for different types, the process of selecting the appropriate instance always converges to a single, unique type), making the process of resolving overloaded identifiers globally unambiguous
```haskell
    class Functor f where --f is a type constructor that takes one type argument
        fmap :: (a -> b) -> f a -> f b --'f a' and 'f b' are containers that hold the value of 'a' and 'b' type

    instance Functor [] where --the where keyword is used to define local bindings, such as fmap
        fmap f [] = []
        fmap f (x:xs) f x : fmap f xs --in this case f x holds a list of x type as f is a []
    
    instance Functor ((,), a) where
        fmap f (a,b) = (a, f b)
```
- This code defines a typeclass 'Functor' and provides instances of the 'Functor' typeclass for two specific types: lists and pairs (a pair is a tuple that contains exactly 2 elements)

## Operators
- In Haskell, infix operators are simply functions and quite often are used in place of alphanumerica names when the function involved combine in common ways are subject to algebraic laws
- Use the keyword infix[associativity] precedence operator to define an infix operator
- The associativity is the default order in which evaluation occurs when precendence is equal
```haskell
infixl 6 + --defines the + operator to have a precedence of 6 and be left associative
infixl 6 -
infixl 7 /
infixl 7 *

infixr 5 ++ --defines the ++ operator to have a precedence of 5 and be right associative
infixr 9 .
```
- Operators can be written in section form
```haskell
(x+) = \y -> x+y --this partially applied function takes an argument 'y' and returns the result by adding 'x' and 'y'
(+y) = \x -> x+y
(+) = \x y -> x+y
```
- Any binary function (a function that takes in two arguments) can be used in infix form by surrounding the name in backticks
```haskell
    (+1) `fmap` [1, 2, 3] --[2, 3, 4], this uses the fmap function in infix form (change of syntax)
```
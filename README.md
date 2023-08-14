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
 - \x -> defines an anonymous function also known as "lambda functions" that take in a parameter 'x'. The -> here is not a type denoter but rather part of the lambda syntax that points to the anonymous function
```haskell
    compose f g = \x -> f (g x) --this function identified by 'compose' takes in two arguments f and g which returns an unnamed function aka a lambda function that takes in one parameter 'x' and returns the composite function f(g(x))
```
- <$> Is 'fmap' in infix operator form, it takes a function (a -> b) and applies it to the value of type 'f a', resulting in a new value of 'f b', this is provided by the Functor class type, it's type signature looks as such
```haskell
    (<$>) :: (a -> b) -> f a -> f b
```
- Here, the f a is unwrapped into the 'a' type, passed into the function as a parameter, a value of 'b' type is evaluated and then rewrapped in the same context 'f'
- $ on the other hand is a basic function application operator that allows you to apply a function to an argument, it's type signature looks like this!
```haskell
    ($) :: (a -> b) -> a -> b
```
- This defines an infix operator '$' that takes in a function which takes in argument type 'a', an argument of type 'a', and evaluated to a value of type 'b', the same value the function evaluates to

## Data types
 - 'data' is a keyword used to define an algebraic data type (sum types and product types)
 - The identifier/name for said data type must begin with a capital letter
 - '|' Define sum types, an algebraic data type that consists of multiple options of type constructors
 - Product types define multiple fields into the same type, like a struct and are defined with a space in between two data types
 - Records are a special product type that generate a special set of functions known as selectors which extract values of a specific field from the Records
```haskell
    data Sum = A int | B Bool --the sum data type with two type constructors
    data Prod = Prod Int Bool --the prod data type with two types, Int and Bool, Prod is the name of the data type
```
- The left side defines the names and arguments of the data type whilst the right side defines its constructors
- Sums and products can be combined
```haskell
    data T1 = A Int Int | B Bool Bool
```
- The 'type' keyword can be used to define an alias of a type, which means the alias type and the original type will be considered the same within type checking
```haskell
    data Colour = RGB (Int, Int, Int)
    type Palette = [Colour]
```
- Here the 'type' Colour constructed with the RGB constructor that takes in three integers as arguments is defined, the line below uses the 'type' keyword to define a type alias Palette which is constructed by a list of colours. This means a function that needs a palette but gets a list of colours is completely valid within Haskell's typechecking

## Values
- Homogeneous: Consisting of 1 data type | Heterogenous -> Consisting of multiple data types
- Inductively defined 'x' type: Constructed by combining simpler cases and a 'x' operator -> e.g Inductively defined sum type is defined with a sum operator like :

- A list is a homogenous inductively defined sum type of linked cells parametrized over the type of its values
```haskell
    (1 : (2 : ( 3: [])))  = [1, 2, 3]
```
- A tuple is a heterogeneous product type parametrized over the types of its two values. Tuples have a special value-level syntax
```haskell
    data myTuple a b = myTuple a b
    myTuple = (1, 2) -> a --tuple is defined by parenthesis and separated with commas
```

## Pattern Matching
- Pattern matching allows us to discriminate on the constructors of a datatype, mapping separate cases to separate code paths and binding variables for each of the fields of the datatype
```haskell
    data Maybe a = Nothing | Just a, this will return different values based on the data type of the argument which can be constructed as Nothing or Just, a sum type 
    
    maybe :: b -> (a -> b) -> Maybe a -> b --this is the type signature for the built in maybe function
    maybe n f Nothing = n --when n is constructed with the Nothing constructor
    maybe n f (Just a) = f a --when n is constructed with the Just constructor
```
- In the piece of code above, the 'maybe' data type has two constructors, 'Nothing' and 'Just', it also takes in two arguments, a function 'f' and a default value 'n'. Pattern matching will match whether the 'Maybe a' was constructed with the 'Nothing' constructor or the 'Just constructor
- In the case that the Nothing constructor was provided, the default value 'n' will be returned, otherwise the function will unwrap the value 'a' and returns the function 'f' applied to the value 'a' as 'f a'
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
- The symbol '|' can be used to add guards (not to be confused with the '|' that declares a sum data type). Guards only allow the execution of a branch if the corresponding predicate yields to True
- One can think of a guard as an conditional statement (if else)
```haskell
    filter :: (a -> Bool) -> [a] -> [a], type definition
    filter pred [] = []
    filter pred (x:xs)
        | pred x = x : filter pred xs, 
        | otherwise = filter pred xs
```
- This constructs a filter with pred as the function and an empty list, the filter function will recurisely call upon the filter function, checking if the 'head' of the list of 'a' type or 'x' fulfills the pred function (pred here is just a placeholder for a predicate function), it the conditions are met, x is pushed back onto the returned list
- 'otherwise' suggests the case that the predicate condition is not met by the 'x' value, and is used specifically in guard statements
- If the input list is non-empty and can be deconstructed into an empty head, then apply the rest of 'filter' based on whether x is true or false. if x is true then 'x' is included in the filtered output list. If x is false it is excluded from the filtered output list

## Recursion
- In Haskell, all iteration over data structures is performed by recursion
- Entering a function in Haskell does not create a new stack frame, the logic of the function is simply entered with the arguments on the stack and yields the result to the register
- In the case where functions return an invocation of itself in tail position (the position in a function where a recurisve call is the last operation before the function returns a result), the resulting logic is compiled via a jump instruction rather than a call
```haskell
    sum :: [Int] -> [Int]
    sum ys = go ys 0
        where
            go (x:xs) i = go xs (i+x)
            go [] i = i
```
- The 'where' keyword defines a block of code local to the enclosing function 
- This sum function takes in a list of integers and outputs a list of integers. 'ys' as stated in the type signature
- The sum function takes in 'ys' a list of integers and calls the 'go' function defined in the 'where' block
- The 'go' function is an auxiliary function (a helper function) that takes in a parameter integer list and an initial accumulator 0, then it uses pattern matching to determine the next course of actions 
- If 'go' takes a parameter of an empty list it returns i, if the parameter can be broken down into a head and a tail, it recursively calls upon the sum function to calculate the final sum. The types for the go function are not explicitly stated

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
- A 'thunk' is a function that has yet to be computed, when you bound a value to a variable, you're not performing the evaluation yet, rather you're creating a thunk that represents that computation. This allows the lazy evaluation to avoid unnecessary calculations
- A thunk that is currently being evaluated may induce the evaluation of other thunks in the process
- An expression in "weak head normal form (WHNF)" refers to a partially evaluated expression where the outermost constructor (data type) has been evaluated but the arguments to the constructor might still be thunks
- The runtime has the task of determining which thunks are to be evaluated by the order in which they are connected to the main function node, this is known as "graph reduction"
```haskell
    ones = 1 : ones --infinite stream of 1's

    numsFrom n = n : numsFrom (n+1) --infinite count from n

    squares = map (^2) (numsFrom 0) --infinite stream of integer squares

    take :: Int -> [a] -> [a] 
    take n _ | n <= 0 = [] --'<=' is a comparison operator not an arrow, _ is used as a placeholder for a value you do not intend to use, in this case anything that is not an integer list!
    take n [] = []
    take n (x:xs) = x : take (n-1) xs

    take 5 squares -- [0, 1, 4, 9, 16]
```
- Infinite streams are possible in Haskell due to laziness, when you define an infinite stream, Haskell does not generate the entire infinite list at once, but rather it generates values on-the-fly as needed
- The 'take' function is used to consume values from infinite streams without evaluating the entire stream, it works efficiently with laziness
- In the example of take 5 square, Haskell only generates 5 squares due to laziness

- In Haskell, a 'bottom' sometimes called a "diverging term" is used to describe a non-terminating or undefined value, often represented as '⊥', it's value has no normal form and does not produce a valid result. It does not successfully compute and yield any meaningful output
- Laziness in Haskell allows for bottoms to be "threaded around" without immediately causing an error or divergence. These values can be passed around through expressions until they are forced to be evaluated
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
- On the left side, a new type named 'T1' that takes in two type parameters 'f' and 'a' is introduced, on the right side, the constructor of the 'T1' type is defined, a single argument of type 'f a'
- To clarify, only the left side will have '->' notation as only function type signatures use the '->' syntax, that is how to distinguish if two values wrap each other or if there are multiple arguments
- The three special types (,), (->), [] have special type-level syntatic sugar
```haskell
    (,) Int Int = (Int, Int) --tuples
    (->) Int Int = Int -> Int --functions
    [] Int = [Int] --lists
```

## Typeclasses
- A typeclass defines a collection of functions which need to conform to the given interface, certain functions are considered 'minimal functions' that need to be defined, whilst the rest of the functions will be derived by Haskell 
- An implementation of an interface is called an 'instance'
- Typeclasses are effectively syntatic sugar for records of functions and nested records (dictionaries) of functions parametrized over the instance type 
- These dictionaries are implicitly threaded throughout the program when an overloaded identifier is used
- When a typeclass is used over a concrete type, the implementation is simply spliced (the call is replaced with the corresponding function) in at the call site
- When a typeclass is used over a polymorphic type (polymorphic types are types that can represent values of different specific types), an implicit dictionary parameter is added to the function so that the implementation of the necessary functionality is passed with the polymorphic value
- Typeclasses are "open" and additional instances can always be added, but the defining feature of a typeclass is that the instance search always converges to a single type (when you have multiple instances of a typeclass for different types, the process of selecting the appropriate instance always converges to a single, unique type), making the process of resolving overloaded identifiers globally unambiguous
- Typeclasses are defined with the 'class' keyword
```haskell
    class Functor f where --f is a type constructor that takes one type argument
        fmap :: (a -> b) -> f a -> f b --'f a' and 'f b' are containers that hold the value of 'a' and 'b' type

    instance Functor [] where --the where keyword is used to define local bindings, such as fmap
        fmap f [] = []
        fmap f (x:xs) f x : fmap f xs --in this case f x holds a list of x type as f is a []
    
    instance Functor ((,), a) where
        fmap f (a,b) = (a, f b)
```
- This code defines a typeclass 'Functor' (a built-in type class in Haskell that represent computations that can be mapped over as Functors are able to utilize the 'fmap' a built-in Haskell function) and provides instances of the 'Functor' typeclass for two specific types: lists and pairs (a pair is a tuple that contains exactly 2 elements)
- This is the signature for the 'fmap' function
```haskell
    fmap :: Functor f => (a -> b) -> f a -> f b
```
- This checks if the f class completes the functor class type, it has two arguments a function that takes in type 'a' and evaluates to type 'b' and a type 'a' wrapped in the type 'f', ultimately evaluating to a 'b' type value wrapped in the 'f' type
## Operators
- In Haskell, infix operators are simply functions and quite often are used in place of alphanumerical names when the function involved combine in common ways are subject to algebraic laws
- Use the keyword infix[associativity] precedence operator to define an infix operator
- The associativity is the default order in which evaluation occurs when precedence is equal
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

## Monads
- A monad is a typeclass with two functions: bind and return
- The primary purpose of monads is to provide a structured way to compose and sequence computations that involve side effects or context, while still maintaining the principles of functional programming and avoiding the use of global variables or mutable state (you will see how!)
- It is important to note that monads are evaluated from left to right

```haskell
    class Monad m where
        bind :: m a -> (a -> m b) -> m b
        return :: a -> m a
```
- This piece of code defines a monad, but a bind is usually written as an infix operator which can be modified to look like
```haskell
    infixl 1 >>=

    class Monad m where
        (>>=) :: m a -> (a -> m b) -> m b 
        return :: a -> m a 
    
```
- This defines the structure, but the monad itself also requires three laws that all monad instances must satisfy
- The bind function or "infix operator" takes a monadic value and a function that takes a monadic value as an argument and returns a monadic value, this new monatic value is often used as an argument to another function that produces another monadic computation
- The return function takes a value and turns it into a monadic value so that monadic functions can be applied to that value

- Law 1: return a >>= f = f a
- This law states that if you wrap a value 'x' using return, turning it into a monadic value and bind it with the function f, the result will be equivalent to directly applying function 'f' to value 'x'

- Law 2: m >>= return = m
- This law states that if you bind a monadic value 'm' with the 'return' function, it's equivalent to the original monadic value 'm'

- Law 3: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
- This law ensures that the order in which you chain computations with the bind function does not affect the final result. It can be thought of as parenthesis can be freely inserted without changing the meaning of the expression statement

- Haskell has a level of syntatic sugar for monads known as do-notation. In this form, binds are written sequentially in block form which extract the variable from the binder
- The => symbol is used in type declarations to indicate constraints or class constraints on type variables. It is used in the context of defining types for functions or data structures to specify additional requirements or capabilities
- The >> operator is used to sequence monadic computations and discard the result of the first computation, it's often used when you're interested in the side effects of a monadic action but don't need to use the result, for example in IO which will be talked about later
- The >> operator has two signatures
'''haskell
    (>>) :: Monad m => m a -> m b -> m b
'''
- This piece of code takes two monadic computations as arguments and returns a new monatic computation as a result
- The => is a class constraint that states that type 'm' must be an instance of the 'Monad' class
- Furthermore, Haskell's 'do' notation is specifically designed for working with monads, when you use 'do' notation 'f' is presumed to represent a monadic value or computation 
- In 'do' notation, code is strictly evaluated rather than lazily evaluated, meaning that the code is executed in runtime from top to bottom as defined in the code structure, in order to have a strict evaluated 'do' notation is imperative, however let functions are still lazily evaluated within a 'do' block
- <- Is a symbol used for binding values from monadic computations (monads), it extracts values from monads and use them in the context of other computations and is commonly utilized in 'do' notation to sequence and combine monadic actions
- The semicolon ';' is used to separate different action or statements within a block, the 'do' notation allows you to sequence and combine monadic actions in a more structured style
- The ultimate purpose of 'do' notation is to write code that performs a series of these monadic actions (computation or operation encapsulated in a monad)
```haskell
    do { a <- f; m } = f >>= \a -> do { m }
    do { f ; m } = f >> do { m }
    do { m } = m
```
- Here you can use 'do' notation on the left side to replace the binding infix operators on the right side
- The first line of code binds the value of the monadic function f to the identifier 'a', while having a separate monadic computation 'm'
- The second line the code performs the monadic function 'f', discards the result (as defined by the >>) and evaluates the monadic computation 'm'. The result is also discarded in 'do' notation as the evaluated value is not binded to an identifier
- The third line you have a single monatic computation, it is equivalent to the monatic computation itself
- For example, the following are equivalent
```haskell
    do
        a <- f
        b <- g
        c <- h
        return (a, b, c)

    f >>= \a ->
        g >>= \b ->
            h >>= \c ->
                return (a, b, c)
```
- This is a more complex line written 'do' notation to add syntatic sugar to the series of monadic operations below. But to understand 'do' notation it is important to understand what the 'do' notation is attempting to act as syntax sugar for. 
- This bottom piece of code uses monadic operations to sequence a series of monatic computations with the ultimate goal of binding and returning the tuple (a, b, c)
- The 'f' is a monadic action that produces a value of type 'a'. The lambda '\a' then takes in 'a' as an argument and feeds it into function, effectively 'storing' the value,
- With 'g >>= \b', 'g' is a monadic action that produces a value 'b'. The lambda '\b ->' captures the value 'b' from the previous action and feeds it to the next function
- Finally in 'h >>= \c ->' the result of the monadic function 'h' is captured as the argument for lambda function and the result of each captured value is returned in a tuple as (a, b, c)
- 'do' notation simplifies this binding process, the monadic function 'f' is binded to the value 'a' while the nomadic function 'g' is binded to the value 'b' and so on. Then all values are returned as (a, b, c)

## Applicatives
- Applicatives are similar to monads in the way that they are both abstractions (the process of simplifiying complex concepts or systems to make the code more readable) that help manage computations that have some sort of context or additional structure
- However, unlike monads, applicatives do not allow you to bind variables and reuse values in the same way monads do
- This limitation do not allow applicatives to bind and send variables between computations
- To be defined as an applicative type class, the function must fulfill the requirement of being a functor type class, the typeclass must also contain the 'pure' function and '<*>' the applicative operator
- <*> Is an operator that applies a function inside a functor to a value that's also wrapped inside an applicative functor, this is provided by the Applicative class type, it's type signature looks as such
```haskell
    (<*>) :: f (a -> b) -> f a -> f b
```
- The important thing to acknowledge about the applicative functor is that it has two discriminators, rather than one like 'fmap'
```haskell
    Just (+1) <*> Just 1
    Just (+1) <*> Nothing
    Nothing <*> Just 1
    Nothing <*> Nothing
```
- This is an example using 'maybe' which has two valid constructors (Just or Nothing), and we have two discriminators, we have four possible cases
- This infix operator acts the exact same as 'fmap' except that the function that takes in (a -> b) is wrapped in the context 'f', adding on an extra discrminator
- Parenthesis enclose functions an allow them to be utilized as infix operators like (<*>) or (<$>)
```haskell
    class Functor f => Applicative f where --specifies that 'f' must be an instance of the 'Functor' type class
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
    
    (<$>) :: Functor f => (a -> b) -> f a -> f b
    (<$>) = fmap
```
- In the context of Haskell and functional proramming a "pure value" refers to an immutable value that exists independently of any computational effects or side effect
- The 'pure' function takes a value and 'lifts' it into applicative context, in other words it creates an applicative functor that wraps the value it lifts, turning it from value 'a' to value 'f a'
- Generally, you can not directly apply an unpure function to a value wrapped in the applicative context when using the <*> operator
- The '.' operator is the function composition operator, it is a built-in operator that takes in two functions as arguments and returns a new function that represents the composition of the two functions
```haskell
    (.) :: (b -> c) -> (a -> b) -> a -> c
```
- Here the (b -> c) is the second function you want to apply while (a -> b) is the first function you want to apply. 'a' is the input value you want to pass into the composite function and 'c' is the result type of the composition
- Applicatives satisfy the following laws
```haskell
    pure id <*> v = v --identity
    pure f <*> pure x = pure (f x) --homomorphism
    u <*> pure y = pure ($ s) <*> u --interchange
    u <*> (v <*> w) = pure (.) <*> u <*> v <*> w --composition
```
- First off, the identity function is a fundamental concept in mathematics and computer science, it's a function that takes an argument and returns the same value, in Haskell the identity function can be defined as
```haskell
    id :: a -> a
    id x = x
```
- The first law of applicatives states that the identity function wrapped in an applicative functor, this function is applied to an 'v' which should be equivalent to 'v' itself. In other words using a value inside of the applicative context does not alter the value wrapped inside of the functor
- The second law expresses the concept of homomorphism, it states that if you lift a pure function 'f' into the applicative context and apply it to a pure value 'x' it should be the same as lifting the result of applying function 'f' to value 'x' directly using 'pure'. The pure function is a lot like the return function of monads in the way that they 'lift' the value into each of their respective contexts
- The third law is the interchange law. This law states applying a function to a lifted value is equivalent to applying a lifted function to a value assuming all values and functors (either a function or a value) reside in the same applicative context. 
- Finally, the initmidating law of composition is just a demonstration of composite functions where the 'u' functor is applied to the value evaluated by the 'v' and 'w' functors. This can be represented with the composite operator by calling (.) and applies each previously evaluated value with the next functor. This also comes to show that the order of composition does not matter, for example the composition of 'v' and 'w' applied to 'u' is equivalent to the composition of the functors 'u' and 'v' applied to 'w', and etc
- For example
```haskell
    example1 :: Maybe Integer
    example = (+) <$> m1 <*> m2
        where
            m1 = Just 3
            m2 = Nothing
```
- Instances of the Applicative typeclass also have the available functions '*>' and '<*', these function sequence applicative actions while discarding the value of one of the arguments. While '*>' discards the left argument '<*' discards the right
- For example in a monadic parser combinator library (a tool that allows you to define and compose parsers using monads), the *> would discard the value of the first argument but return the value of the second

## Monoids
- Monoids provide an interface for structures which have an associative operator (mappend, which can be represented as an infix operator as <>) and a neutral element (mempty) for that operation
- Mappend or <> is a function that represents the binary operation of a monoid, it takes two elements of the monoid and combines them to produce a third element of the same monoid
- The neutral element, also known as the identity element is a special element within a monoid. When this element is combined with any other element using the monoid's operation, it doesn't change the other element, in other words
```haskell
    mappend x e = x
```
- 'mempty' is a function that returns the neutral element of a monoid. In Haskell, the 'mempty' function provides a way to access the neutral element of a monoid type, for example a monoid that represents a list concatenation, 'mempty' would be the empty list '[]' which acts as the neutral element for list concatenation
- A monoid class can be defined as such
```haskell
    class Monoid a where
        mempty :: a
        mappend :: a -> a -> a
        mconcat :: [a] ->
```
- 'mconcat' for lists takes a list of lists and concatenates them into a single list [[1, 2], [3, 4]] produces [1, 2, 3, 4]
- The canonical example is the list type with concatenation as the operation and the empty list as zero
```haskell
    import Data.Monoid

    a :: [Integer]
    a = [1, 2, 3] <> [4, 5, 6]

    b :: [Integer]
    b = ([1, 2, 3] <> mempty) <> (mempty <> [4, 5, 6])
```
- The first line applies the 'mappend' function in infix form to two lists, [1, 2, 3] and [4, 5, 6], combining two elements of the monoid and creating a third element
- The second line applies the 'mappend' function in infix form to the neutral element or 'mempty' which, in this case is an empty list. This returns the value of the list itself, finally the the 'mappend' function is called on the two evaluated values combining monoid elements [1, 2, 3] and [4, 5, 6] to form the singular monoid element [1, 2, 3, 4, 5, 6]

## Deriving
- Instances for typeclasses like 'Read', 'Show', 'Eq', and 'Ord' can be derived automatically by the Haskell compiler
```haskell
    data PlatonicSolid
    = Tetrahedron
    | Cube
    | Octahedron
    | Dodecahedron
    | Icosahedron
    deriving (Show, Eq, Ord, Read)

    example = show Icosahedron
    example = read "Tetrahedron"
    example = Cube == Octahedron
    example = sort [Cube, Dodecahedron]

```
- Essentially, by using the keyword 'deriving', you are instructing the Haskell compiler to automatically generate implementations of the 'Read', 'Show', 'Eq', and 'Ord' typeclasses for this data type, allowing values of this data type to use functions associated with these typeclasses, for example the 'show' function that will implement itself differently based on the argument provided

## IO (Input/Output)
- In Haskell, the IO type represents computations that involve input and output operations
- A value of type IO 'a' is a computation which, when performed, does some I/O before returning a value of type 'a'.
- The notable feature of Haskell is that IO is still functionally pure, a value of type IO 'a' is simply a value which stands for a computation which, when evoked will perform IO, there is no way to peek into its contents without running it
- For instance, the following function does not print the numbers 1 to 5, instead it builds a list of IO computations:
'''haskell
    fmap print [1..5] :: [IO ()]
'''
- Here, 'fmap' applies the 'print' function to each element in the list, creating a list of IO computations that each print a number and return a unit value '()', which represents no useful results
- We can then manipulate them as an ordinary list of values:
'''haskell
    reverse (fmap print [1..5] :: [IO ()])
'''
- In this line, the IO computations are reversed, it is important to note that the list does not actually perform I/O operations, it only rearranges the order of computations in the list
- To actually execute the 'IO' actions and perform the I/O operations, the sequence_ function is used, which takes a list of 'IO' actions and evaluates them from left to right, discarding the results, the result is discarded as only the side effect is off interest (>>) will be useful here. The type signature of a sequence_ function looks as such:
```haskell
    sequence_ :: (Monad m) => [m a] -> m ()
```
- sequence_ takes a list of monadic actions '[m a]' and returns a monadic action 'm ()', a computation with monadic context that does not produce any meaningful value where 'm' is any monad (including the IO monad)
- The resulting IO computation can be evaluated in main which is typically defined as an IO action with a 'do' block, allowing IO actiins to bind values to variables (or the GHCi repl, which effectively is embedded inside of IO)
'''haskell
    >> sequence_ (fmap print[1..5]) :: IO ()
    1
    2
    3
    4
    5
'''
- The resulting computation combines the list of 'IO' actions into a singular 'IO' action that represents the sequence of the input 'IO' actions
- Applying the reverse function onto the type IO []int will result in the exact same result with the sequence reversed
'''haskell
    >> sequence_ (reverse (fmap print [1..5]) :: IO ())
    5
    4
    3
    2
    1
'''
- The IO monad is wired into the runtime with compile support, it is a special case and most monads in Haskell have nothing to do with effects in this sense
```haskell
    putStrLn :: String -> IO ()
    print :: Show a => a -> IO ()
```
- 'IO ()' represents an IO action, this is because actions like 'putStrLn' and 'print' can not be functions, as functions in Haskell have to be pure
- In order to get the result of an IO action, the IO action must be binded to a value inside of a variable inside of another IO action
- Here, the function 'putStrLn' takes in a string and returns values of type IO (), which represent actions that perform side effects and has no significant return value
- 'print' is similar to 'putStrLn' except that it only takes in a value of type 'a' if it implements the 'Show' typeclass rather than just any string
- The type of main is always IO ()
```haskell
    main :: IO ()
    main = do
        putStrLn "Enter a number greater than 3: "
        x <- readLn
        print(x > 3)
```
- In Haskell, a program typically has a main function of type 'IO ()', this function servers as the entry point to your program and is where you sequence and compose various 'IO' actions to achieve the desired effect
- Here, 'do' notation is utilized to bind the variable from readLn to the variable 'x', this whole function prints the prompt "Enter a number...", binds the value to the variable 'x' and prints the result of the evaluation x > 3
- The essence of monadic IO in Haskell is that effects are reified as first class values (the concept of IO which is typically considered a side effect is made concrete and represented as a first class value' in the language and reflected in the typesystem, this is one of the foundational ideas of Haskell, though not specific to Haskell

## Monad Transformers
- Monads can be combined together to form composite monads
- Each of the composite monads consists of layers of different monad functionality
- An example of this is combining an error-reporting monad with a state monad to encapsulate a certain set of computations that need both functionalities
- The use of monad transformers while not always necessary is often one of the primary ways to structure modern Haskell programs
```haskell
    class MonadTrans t where
        lift :: Monad m => m a -> t m a
```
- This typeclass has one function it needs to implement, the 'lift' function. The 'lift' function takes a computation 'm a' in the base monad 'm' and lifts it into a monad transformer 't', effectively enabling the value to deal with computations involving multiple monad layers
- The implementation of monad transformers is comprised of two complementary libraries, 'transformers' and 'mtl'. The 'transformers' library provides the monad transformer layers and 'mtl' extends this functionality to allow implicit lifting between several layers
- To use the transformers, we simply import the 'Trans' variants of each of the layers we want to compose and then wrap them in a newtype
- 'newtype' is a keyword that is used to define a new type with a single constructor and a single field (a field is a piece of data associated with the constructor), the new type and the type of the field are in direct correspondance (isomorphic)
- types defined with 'newtype' are checked at compile time yet ignored during runtime (at runtime it is treated as a string), also no additional matching is needed during pattern matching as there can only be one constructor
```haskell
    import Control.Monad.Trans
    import Control.Monad.Trans.State
    import Control.Monad.Trans.Writer

    newtype Stack a = Stack { unStack :: StateT Int (WriterT [Int] IO) a }
        deriving (Monad)
    
    foo :: Stack ()
    foo = Stack $ do
        put 1 -- State layer
        lift $ tell [2] -- Writer layer
        lift $ lift $ print 3 -- Io Layer
        return ()
    
    evalStack :: Stack a -> IO [Int]
    evalStack m = execWriterT (evalStateT (unStack m) 0)
```
- The code first imports from necessary libraries. 'Code.Monad'Trans' provides the monad transformer types, and 'Control.Monad.Trans.State' and 'Control.Monad.Trans.Writer' are specific monad transformer layers to be 'layered'
- Next a 'newtype' named 'Stack' is declared, parametrized by a type variable 'a', in this case 'Stack' is a wrapper around a monad transformer stack
- The single constructor of the 'Stack' newtype is then defined, it uses record syntax to label the constructor field as 'unStack' which hold the actual value of the monad transformer stack 
- 'StateT Int (WriterT [Int] IO) a' is the type of the monad transformer stack being wrapped by the 'Stack' newtype, it is a composition of three monads
- 'IO' is the outermost monad in the stack, which allows for I/O operations, WriterT is the middle monad, parametrized by [Int] as the type to accumulate, lastly State T is the innermost monad transformer parametrized with an 'Int' as the state type, the 'a' at the end represents the type of value produced by the computation
- Then the 'deriving (Monad)' line uses the 'deriving' keyword to automatically create an instace of the 'Monad' typeclass for the 'Stack' newtype effectively allowing the use of monadic operations such as 'return', '>>=' directly with values of type 'Stack a'
- The purpose of the 'unStack' function is to allow you to unwrap and access the underlying computations within the 'Stack' newtype
- Next the 'foo' function is defined as a 'Stack ()' type, meaning it deals with side effects and has no significant return value, the purpose of this function is to demonstrate the use of the 'Stack' monad
- put 1 updates the state in the 'State' layer
- 'lift $ tell [2]' appends a value to the writer, however, it is visible that the value needed to be lifted further into the monad transformer in order to access the tell function, part of the WriterT monad transformer. It has to be lifted one more time in order to access the outermost layer to perform an IO action with the 'print' function', 'return ()' then wraps up the computation with a result
- Finally evalStack takes a computation in the 'Stack' monad and evaluates it, returning the list of accumulated values written to the 'Writer' layer using evalWriterT and evalStateT to extract the results from each respective layer
- Using 'mtl' and 'GeneralizedNewTypeDeriving' we can produce the same stack but with a simpler interface to the transformer stack, under the hood 'mtl' is using an extension called 'FunctionalDependencies' to automatically infer which layer of a transformer stack a function belongs to and can then lift the function into it

```haskell
    import Control.Monad.Trans
    import Control.Monad.State
    import Control.Monad.Writer

    newtype Stack a = Stack { unStack :: StateT Int (WriterT [Int] IO) a }
        deriving (Monad, MonadState Int, MonadWriter [Int], MonadIO)

    foo :: Stack ()
    foo = do
        put 1
        tell [2]
        liftIo $ print3
        return ()
    
    evalStack :: Stack a -> IO [Int]
    evalStack m = execWriterT (evalStateT (unStack m) 0)
```
### StateT
- The state monad allows functions within a stateful monadic context to access and modify shared state, a 'state' is a piece of data that can be accessed and modified by various computations in a controlled manner
- Below is a list of functions and their type signatures that come with the StateT monad transformer
```haskell
    put :: s -> State s () -- set the state value
    get :: State s s --get the state value
    gets :: (s -> a) -> State s a --apply a function over the state and return the result
    modify :: (s -> a) -> State s () --set the state using a modifier function
```
- In 'State s s', the first 's' refers to the type of the state being accessed while the second 's' refers to the value being retrieved from the state monad 
- It is also important to note that States store one value at a time
- Evaluation functions often follow the naming convention of using the prefixes run, eval, and exec:
```haskell
    execState :: State s a -> s -> s --yield the state
    evalState :: State s a -> s -> a --yield the return value
    runState :: State s a -> s -> (a, s) --yeld the state and return value
```
- An example of this can be seen in the following piece of code
```haskell
    import Control.Monad.State

    test :: State Int Int
    test = do
        put 3
        modify (+1)
        get
    
    main :: IO ()
    main = print $ execState test 0
```
- This function sets the state to 3, modifies it with the add 1 function and gets the state value, then the main (defined as an IO action) prints the result of execState to execute the 'test' computation with an initial state of '0'

### ReaderT
- The Reader monad allows a fixed value to be passed around inside the monadic context
```haskell
    ask :: Reader r r --get the value
    asks :: (r -> a) -> Reader r a --apply the function to the value and return the result
    local :: (r -> r) -> Reader r a -> Reader r a --run a monadic action with the value modified
```
- An example of this can be seen in the following piece of code
```haskell
    import Control.Monad.Reader

    data MyContext = MyContext
        {
            foo :: String
            bar :: int
            deriving (Show)
        }

    computation :: Reader MyContext (Maybe String)
    computation = do
        n <- asks bar
        x <- asks foo
        if n > 0
            then return (Just x)
            else return Nothing
    
    ex1 :: Maybe String
    ex1 = runReader computation $ MyContext "hello" 1
    
    ex2 :: Maybe String
    ex2 = runReader computation $ MyContext "haskell" 0
```
- MyContext is a data type with a single constructor 'MyContext' that takes two fields 'foo' of type string and 'bar' of type int
- MyContext is also an environment with two fields
- '{}' in a data type definition indicates that record syntax is being used and the field names listed within curly braces are automatically field accessor functions
- A field accessor function is a function that allows you to retrieve a value of a specific field, 'bar' and 'foo' are field accessor functions
- So when 'computation' is defined using the 'Reader' monad, the 'bar' accessor function is applied to the environment 'MyContext' and retrieves the 'Int' value stored in the 'bar' field, assigning it to the variable 'n', the same happens for the accessor function 'foo' and variable 'x'
- It then checks whether 'n' is greater than 0, if it is it returns Just x otherwise it returns 'Nothing'
- runReader will run the following computation where the context, string and integer are applied to the function 'computation' and set ex1 and ex2 as each respective result

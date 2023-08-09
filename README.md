# Haskell Basics

# Syntax
 - :: Is a type annotation used to specify the type of an expression
 - -> Is used to denote function types, the last -> denotes the return type of the function
 - The syntax will look like identifier :: arg1 type -> arg2 type -> return type

 - : The cons operator is used to instruct a new list by adding an element in front of the list a "push" operator
 - The syntax looks like (1 : (2 : ( 3: [])))  = [1, 2, 3]

# Functions
 - Functions are first class citizens in Haskell that can be passed around as an argument and yielded as a return value
 - compose f g = \x -> f (g x), this function identified by "compose" takes in two arguments f and g which returns an unamed function aka a lambda function and returns the composite function f(g(x))

# Data types
 - "data" is a keyword used to define an algebraic data type (sum types and product types)
 - "|"" Define sum types, an algebraic data type that consists of multiple options of type constructors
 - Product types define multiple fields into the same type, like a struct 
 - Records are a special product type that generate a special set of functions known as selectors which extract values of a specific field from the Records
    data Sum = A int | B Bool -> Sum data type with two type constructors
    data Prod = Prod Int Bool -> Prod data type with two types, Int and Bool, Prod is the name of the data type

- Sums and products can be combined
    data T1 = A Int Int | B Bool Bool

# Values
## Definitions:
- Homogeneous: Consisting of 1 data type | Heterogenous -> Consiting of multiple data tyeps
- Inductively defined "x" type: Constructed by combining simpler cases and a "x" operator -> e.g Inductively defined sum type is defined with a sum operator like :

- A list is a homogenous inductively defined sum type of linked cells parametrized over the type of its values
    (1 : (2 : ( 3: [])))  = [1, 2, 3]
- A tuple is a heterogeneous product type parametrized over the types of its two values. Tuples have a special value-level syntax
    data myTuple a b = myTuple a b
    myTuple = (1, 2) -> a tuple is defined by paraenthesis and separated with commas

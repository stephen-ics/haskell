# Lambda Calculus
- The lambda calculus consists of three terms and all valid recursive combinations thereof
```
    (λx.x)(λy.y)
```
- The three terms are typically referred to in code by several contrations of their names:
- Var: A variable
- Lam: A lambda abstraction
- App: An application
- In the code above, 'x' is the variable 'λx.x' is the lambda abstraction while the application process involve substituting the argument for the parameter throughout the function's body
- A lambda term is said to 'bind' its variable, for example the lambda here binds 'x'
```
    f = λx.e
```
- In mathematics we would typically right
```
    f(x) = e
```
- In other words λx.e is a function that takes in a variable 'x' and returns 'e'
- We will be discussing untyped lambda calculus and move into typed lambda calculus

## Syntactical Conventions
- Several syntactical convetions will be adopted when writing lambda expressions
- Firstly, application of multiple expressions associates to the left
```
    x1x2x3...xn = (...((x1x2)x3)...xn)
```
- Parenthesis are used to disambiguate
- In lambda calculus, each lambda abstraction binds a single variable, the lambda abstraction's body may be another lambda abstraction
- Out of convenience, multiple lambda abstractions are written with their variables on one lambda symbol, this is merely a syntactical convention and does not change the underlying meaning
```
    λxy.z = λx.λx.z
```
- The actual implementation of lambda calculus admits several degrees of freedom in how lambda abstractions are represented
- The most notable is the choice of identifiers for the binding variables
- A variable is said to be 'bound' if it is contained in a lambda expression of the same variable binding, conversely a variable is 'free' if it is not bound
- A term with free variables is said to be an 'open term' while one without free variables is said to be 'closed' or a 'combinator'
```
    e0 = λx.x
    e1 = λx.(x(λy.ya)x)y
```
- Here, e0 is a combinator while e1 is not, in e1 both occurences of 'x' are bound, while the second is free, 'a' is also free
- Multiple lambda abstractions can be bind to the same variable name, each occurance of a variable is then bound by the nearest enclosing border
```
    λxy.(λxz.x+y)
```
- For example the variable 'x' in the expression above is bound on the inner lambda, while 'y' is bound on the outer lambda, this is referred to as 'name shadowing', in other words, the inner variable takes precedence over the outer variable 'shadowing' it
- When 'x' is used inside the inner lambda, it refers to the 'x' parameter of the inner lambda, not the 'x' parameter of the outer lambda

## SKI Combinators
- There are three fundamental closed expressions called SKI combinators
```
    S = λf.(λg.(λx.fx(gx)))
    K = λx.λy.x
    I = λx.x
```
- In Haskell, these are written simply as
```haskell
    s f g x = f x (g x)
    k x y = x
    i x = x
```
- Moses Schönfinkel showed that all closed lambda expressions can be expressed in terms of only the 'S' and 'K' combinators, even the 'I' combinator, for example one can show that 'SKK' reduces to 'I'
```
    SKK
    = (λxyz.xz(yz))(λxy.x)(λxy.x)
    = λz.((λxy.x)z)((λxy.x)z)
    = λz.(λy.z)(λy.z)
    = λz.z
    = I
```
- So... what on Earth did we just see... Well, it's not so bad if you break it down step by step!
- 'λxyz.xz(yz)' takes in three parameters, 'x', 'y', and 'z', in this case two of the three arguments are provided, where 'x' is the first (λxy.x) while 'y' is the second (λxy.x)
- After applying these variable bindings to the body of the function we get λz.((λxy.x)z)((λxy.x)z)
- Now both functions '(λxy.x)' have an argument 'z', after applying the argument 'z' to both functions we get 'λz.(λy.z)(λy.z)'
- (λy.z) takes in one argument 'y' while, this argument is the second '(λy.z)', after subsituting 'y' for '(λy.z)' in the body of the function, we get 'z', as 'y' is not utilized, resulting in λz.z, which... if we check above is actually equivalent to 'I'! How cool is that!
- This fact is a useful sanity check when testing an implementation of the lambda calculus

## Implementation
- The simplest implementation of the lambda calculus syntax with named binders is the following definition
```haskell
    type Name = String

    data Expr
        = Var Name
        | App Expr Expr
        | Lam Name Expr
```
- There are several lexical syntax choices for lambda expressions, we will simply choose the Haskell convention which denotes the lambda by the backslash (\) to the body with (->), and application by spaces (where you apply a function to its argument simply by writing them next to each other, separated by a space)
- Named variables are alphanumeric sequences of characters
- Logical notation: 
``` 
    const = λxy.x
```
- Haskell notation:
```
    const = \x y -> x
```
- In addition, terms like literal numbers of booleans can be added, these make writing examples easier
- For these terms, we will add a 'Lit' constructor
```haskell
    data Expr
        = ...
        | Lit Lit
    
    data Lit
        = Lint Int
        | LBool Bool
```
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
```haskell
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

## Substitution
- Evaluation of a lambda term ((λx.e)a) proceeds by substitution of all free occurences of the variable 'x' in 'e' with the argument 'a'
- A single substitution step is called a 'reduction'
- We write the substitution application in brackets before the expression it is to be applied over, [x/a]e maps the variable x to the new replacement 'a' over the expression 'e'
```
    (λx.e)a -> [x/a]e
```
- A substitution metaveriable will be written as [s]
- In detail, a substitution is defined like this
```
    [x/a]x = a
    [x/a]y = y, if x != y
    [x/a]ee' = ([x/a]e)([x/a]e')
    [x/a]λx.e = λx.e
    [x/a]λy.e = λy.[x/a], if x != y and y is not in fv(a)
```
- fv(e) is the set of free variables in the expression 'e'
- the last two define scope cases where 'x' is overriden by the inner λx and how 'y' is not overriden unless 'y' is in a set of free variables in 'e' or x = y, in which case the 'y' in the expression is overriden by the inner 'y'
- The fundamental issue with using locally named binders is the problem of 'name capture' or how to handle the case where a substitution conflicts with the names of free variables
- We need the condition in the last case to avoid the naive substitution that would fundementally alter the meaning of the following expression when 'y' is rewritten to 'x'
```
    [y/x](λx.xy) -> λx.xx
```
- By convention, it is always good to utilize 'capture-avoiding' substitution
- Substitution will only proceed if the variable is not in the set of free variables of the expression, and if it does then a fresh variable will be created in its place
```
    (λx.e)a -> [x/a]e, if x is not a set fv(a)
```
- There are several binding libraries and alternative implementations of the lambda calculus syntax that avoid these problems, it is a very common problem and is very easy to implement incorrectly, even for experts!

## Conversion and Equivalences
### Alpha Equivalence
```
    (λx.e) α= (λy.[x/y]e)
```
- Alpha equivalence is the property (when using named binders) that changing the variable on the binder and throughout the body of the expression should not change the fundemental meaning of the whole expression, so the following are alpha-equivalent
```
    λxy.xy α= λab.ab
```

### Beta-Reduction
- Beta reduction is simply a single substitution step, replacing a variable bound by a lambda expression with the argument to the lambda throughout the body of the expression
```
    (λx.a)y β→ [x/y]a
```
- This is justified by the fact that if we apply both sides to a term, one step of beta reduction turns the left side to the right side
```
    (λx.ex)e′ β→ ee′, if x /∈ fv(e)
```

### Eta-Reduction
- Eta reduction is the process of simplifiying a lambda abstraction by removing uncessary lambda expressions
```
    λx.ex η→ e if x /∈ fv(e)
```
- If you have a lambda abstraction that takes an argument and immediately applies it to a function, you can simplify it by removing the argument and applying the function directly, consider the case as depicted above where a lambda abstraction takes an argument 'x' and immediately applies to the a function 'g'
- This situation is a candidate for eta reduction as we can simplify it by removing the redundant parameter 'x' and applying it to 'g' directly
- This is based on the principle that if a lambda abstraction immediately applies a function to its argument, we can simplify it to just the function itself

### Eta-Expansion
- The opposite of eta reduction is eta expansion, which takes a function that is not saturated (it does not have all of its expected arguments) and makes all variables explicitly bound in a lambda, eta expansion will be important when we discuss translation into STG
```haskell
    add :: Int -> Int -> Int
    add x y = x + y

    addEtaExpand :: Int -> Int -> Int
    addEtaExpand x = \y -> x + y
```
- Here, the second argument 'y' is explicitly bound in a lambda expression

## Reduction
- Evaluation of a lambda calculus expression proceeds by beta reduction. The variables bound in a lambda are substituted across the body of the lambda
- There are several degrees of freedom in the design space about how to do this, and in which order an expression should be evaluated
- For instance, we could evaluate under the lambda expression and then substitute variables into it, or we could evaluate the arguments and then substitute and then reduce the lambda expression
- More of this will be discussed in the section on Evaluation models
```
    Untyped> (\x . x) 1
    1

    Untyped> (\x y . y) 1 2
    2

    Untyped> (\x y z . x z (y z)) (\x y . x) (\x y . x)
        => \x y z . (x z (y z))
        => \y z . ((\x y . x) z (y z))
            => \x y . x
            => \y . z
            => z
        => \z . z
```
- Note that the code evaluated above was SKK which was encountered earlier
- In the untyped lambda calculus we can freely represent infinitely diverging expressions
```
    Untyped> \f . (f (\x . (f x x)) (\x . (f x x)))
```
- Here, the function 'f' is applied to two arguments '(\x . (f x x))', and '(\x . (f x x))', both self-application of 'f' to two arguments 'x' and 'x'
- This is an expression that takes a function 'f' and applies it to two instances of itself, each involving self-application
- This creates a cyclic structure of self application that can lead to infinitely diverging computations
```
    Untyped> (\x . x x) (\x . x x)
```
- The example above is a more direct example, when '(\x . x x)' is subsituted into the body as 'x', the new expression becomes (\x . x x)(\x . x x), creating an infinite stream of same expression
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
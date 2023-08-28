# Type Systems
- Type systems are a formal language in which we can describe and restrict the semantics of a programming lanugage
- This is an extremely large topic, so this section will only cover enough of it to get through writing the typechecker in Haskell, a more in detailed resource is 'Types and Programming Lanugages (TAPL)'

## Rules
- In the study of programming language semantics, logical statements are written in a specific logical notation
- A property, for our purposes will be a fact about the type of a term
```
    1 : Nat
```
- These facts exist within a preset universe of discourse called a 'type system', with definitions, properties, conventions, and rules of logical deduction about types and terms
- Within a given system, we will have several properties about these terms, for example:
    - (A1) 0 is a natural number
    - (A2) For a natural number n, succ(n) is a natural number
Given several properties about natural numbers, we'll use a notation that will allow us to chain them together to form proofs about arbitrary terms in our system
```
    ------- (A1)
    0 : Nat
    
    n : Nat
    ------------- (A2)
    succ(n) : Nat
```
# Type Systems
- Type systems are a formal language in which we can describe and restrict the semantics of a programming lanugage
- This is an extremely large topic, so this section will only cover enough of it to get through writing the typechecker in Haskell, a more in detailed resource is 'Types and Programming Lanugages (TAPL)'

## Rules
- In the study of programming language semantics, logical statements are written in a specific logical notation
- A property will be a fact about the type of a term
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
- An axiom is a fundemental statement or assumption in a formal system that is accepted without proof
- Axioms serve as the foundation upon which the rest of the mathematical or logical system is built
- Here (A1) and (A2) represent the axiomis that state that 0 is a natural number, and that if 'n' is a natural number then all numbers succeeding 'n' is also a natural number
- In this notation, the expression above the line is called 'antecedent', and the expression below the line is called 'conclusion'. A rule with no antedecent (expression above the line) is an axiom
- The variable 'n' is a metavariable standing for any natural number, an instance of a rule is a substitution of values for these metavariables
- A 'derivation' is a tree of rules of finite depth, we write '⊢ C' to indicate that there exists a derivation whose conclusion is 'C' and that 'C' is provable
For example ⊢ 2 : Nat by the derivation
```
    ---(A1)
    0 : Nat
    ---------(A2)
    succ(0) : Nat
    ------------------- (A2)
    succ(succ(0)) : Nat
```
- 'Γ' represents a 'typing context' or a 'typing environment'
- The context is a sequence of named variables mapped to properties about the named variable
- The comma operator for the context extends Γ by adding a new property on the right of the existing set
- The empty context is denoted ∅ and is the terminal element (final element) in this chain of properties that carries no information
- So contexts are defined by
```
    Γ ::= ∅
        Γ, x : τ
```
- The ',' is used to extend the context to add a new variable 'x' of type 'τ'
- Here is an example for a typing rule for addition using contexts:
```
    Γ ⊢ e1 : Nat Γ ⊢ e1 : Nat
    -------------------------
    Γ ⊢ e1 + e2 : Nat 
```
- In this case, where the property is always implied regardless of the context, we can shorten the expression
```
    ∅ ⊢ P := ⊢ P
```
- This implies that the statement 'P' holds true in the general sense without needing to explicitly empty the context every time

## Type Safety
- In the context of modeling the semantics of programming languages using this logical notation, we often refer to two fundamenetal categories of rules of the semantics
    - Statics: Semantic descriptions which are derived from the syntax of a language (type checking, scoping)
    - Dynamics: Semantics descriptions which describe the value evolution resulting from a program (computations and state changes)
- Type safety is defined to be the equivalence betweeen the statics and the dynamics of the language
- This equivalence is modeled by two properties that relate the types and evaluation semantics
    - Progress: If an expression is well typed then either it is a value, or it can be further evaluated by an available evaluation
    - Preservation: If an expression 'e' has type 'τ', and is evaluated to e', then e' has type τ

## Types
- The word 'type' is quite often overload in the common programming lexicon, - Other languages often refer to runtime tags present in the dynamics of the language as "types"
- The ambiguity arises because the term 'type' is used both for the static type information determined at compile time and for the runtime type information associated with values or objects, here are some examples
```
    # Python
    >>> type(1)
    <type 'int'>

    # Javascript
    > typeof(1)
    'number'

    # Ruby
    irb(main):001:0> 1.class
    => Fixnum

    # Julia
    julia> typeof(1)
    Int64

    # Clojure
    user => (type 1)
    java.lang.Long
```
- Types will be written as τ and can consist of many different constructions to the point where the type language may become as rich as the value level language
- For now, let's only consider two 'ground types' (Nat and Bool) and an 'arrow type'
```
    r ::= Bool
          Nat
          r -> r
```
- The arrow type will be the type of function expressions, the left argument being the input type and the output type on the right, the arrow type will be right associative
```
    τ1 -> τ2 -> τ3 -> τ4 = τ1 -> (τ2 -> (τ3 -> τ4))
```
- In all the languages that will be discussed, the types present during compilation are erased
- Although types are possibly present in the evaluation semantics, the runtime cannot dispatch on types of values at runtime
- Types by definition only exist at compile-time in the static semantics of a language



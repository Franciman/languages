The language
------------

As it is customary we give two grammars for the language,
one that is richer,more concrete and apparently more expressive;
and another one that is reduced to the minimum.
We describe how to convert one grammar into another.
The first grammar is the grammar in which we are going to write programs,
the second grammar is the one we are going to reason about for giving semantics
and proving things.


Remember, this is an expression-based language. But we'd like to add some statements
like top level declarations to make things smoother.

Types grammar
==============

```
T -> Bool
   | T -> T
```

The rich grammar
================

```
Expr -> IfExpr
      | FunctionExpr
      | FuncApplication Expr Expr
      | (Expr)
      | Literal
      | Identifier
      
IfExpr -> if Expr { Expr } else { Expr }
FunctionExpr -> [ArgList] Expr
ArgList -> identifier : T OtherArgs
         | €
OtherArgs -> , ArgList         
           | €
Literal -> true | false
```


The poor grammar
===============

```
t -> t t                    ; Function application
   | [x : T]t               ; Lambda abstraction
   | x                      ; Variable
   | true                   ; true literal
   | false                  ; false literal
   | if t then t_1 else t_2 ; if expression
```

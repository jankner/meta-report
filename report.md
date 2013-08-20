\newcommand{\TODO}[1]{\(\spadesuit\){\bf TODO:} {\bf \color{red} #1}\\}
% Meta-repa
% Johan Ankner
%

# Introduction

	

# Theory

## EDSL

A Domain Specific Language (DSL) is a language that is tailored to a
specific domain. Examples of DSLs: SQL for relational database
queries, ...


An Embedded DSL (EDSL) is a DSL that is embedded into a general
purpose language as a library. 

\TODO{explain shallow vs deep embedding?}

\TODO{monomorphic/strict}

## Unboxed types

Most types in GHC have a boxed representation, which means that they
are represented as a pointer to a heap object. The boxed
representation is used to implement polymorphism and laziness. Since
all boxed types are represented by a pointer to a heap object they all
have a uniform representation and can be treated the same way to
support polymorphism. The boxed representation is also indirect, which
is used to support laziness. (..).

Unboxed types are simply represented by the value itself and are not
stored on the heap. In GHC unboxed types, operations and values have a
`#` suffix by convention. They correspond to the primitive types in C.
`Int#` corresponds to `long int`, `Double#` corresponds to `double`
and so on.

Unboxed types have several restriction on their use, but the primary
restriction that is relevant to this thesis is that unboxed types
cannot be passed to a polymorphic function or used with a polymorphic
data type. The reason is that polymorphic arguments and constructor
fields are assumed to be pointers, which unboxed types generally are
not.

Unboxed tuples are used for functions that return multiple values. The
syntax for unboxed tuples is as follows:

~~~
(# e_1, ... , e_n #)
~~~

where `e_1` ... `e_n` are expressions of any type, boxed or unboxed. 

\TODO{inlining}

## Loop fusion

Loop fusion is an optimization where multiple loops are fused into a
single loop. \TODO{why?}

In meta-repa loop fusion is not an optimization that is performed on
the AST. Rather, the array representations and combinators in the
library are defined so that loops are fused by default. For example,
the `map` combinator for Pull arrays are defined using function
composition with the indexing function.

~~~
map f (map g xs)  ===
map (f . g) xs
~~~

Sometimes the programmer wants to prevent fusion. This can be done
with the `force` function, which writes a delayed array to memory. 

## Template Haskell

Template Haskell is a compiler extension that allows template
metaprogramming in Haskell. The programmer writes a program that
is run at compile-time and generates a Haskell AST which can be
inserted (spliced) into a Haskell program.

The meta-repa core language is translated into Haskell using Template
Haskell...

\TODO{GADTs?}



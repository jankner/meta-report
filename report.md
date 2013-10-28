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

## Generalized algebraic data types

Types declared using the keyword `data` in Haskell are known as
algebraic data types (ADTs). Generalized algebraic data types (GADTs) is an
extension to ADTs which allows the programmer to specify the type of
the constructs of the constructors of the type. This sections explains
how this is useful when defining EDSLs.

Consider a simple embedded language which has booleans and integers,
addition of integers and `if`s. In Haskell we can represent the
language with the following ADT:

~~~
data Expr = B Bool
						I Int
						If Expr Expr Expr
						Plus Expr Expr
~~~

Say we wish to write an evaluation function for our language. The
result of an expression can either be an `Int` or a `Bool`, so we will
need to return `Either Int Bool` or something like it from the
evaluation function.

But what should our evalution function return if, for example, we try
to add a boolean expression with a integer expression? We need to be
able to represent errors in the return type, so we change it to `Maybe
(Either Int Bool)`.

This will work, but we only know at runtime what type the result of an
expression will be and we need to handle runtime type errors in the
embedded language. It would be useful if we could determine both the
result type and the well-typedness of the expression at compile-time.
GADTs allows us to use Haskell's type system to do just that.

GADTs let us give custom types to the constructors of our type. This
allows us to express the typing rules of the embedded language in the
Haskell's type system. Haskell's type checker will then disallow any
program that might result in a badly typed term of the embedded
language.

~~~
data Expr a where
	B    :: Bool -> Expr Bool
	I    :: Int -> Expr Int
	If   :: Expr Bool -> Expr a -> Expr a -> Expr a
	Plus :: Expr Int -> Expr Int
~~~

The type-variable `a` in this GADT is a so-called phantom type. It's
called a phantom type because an `Expr a` doesn't contain `a`s like
for example `[a]`. The type is only there to encode the result type of
the expression.

Now that we the result type of the expression is encoded in the type
of the expression we can write an evaluation function of with the
type `Expr a -> a`. No need to check either the well-typedness or the
result type of expression at runtime.

As is often the case when we get more type-safety certain programs are
somewhat more difficult to write. For example, if we want to parse a
string into an expression of our language we have to first parse it
into something like our first representation and then make a function
which type checks the expression and generates the type-safe
representation of the expression if the type checking succeeds.

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




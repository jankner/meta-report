\newcommand{\TODO}[1]{\(\spadesuit\){\bf TODO:} {\bf \color{red} #1}\\}
% Meta-repa
% Johan Ankner
%

# Introduction

	

This thesis presents a new method for writing high-performance
programs in Haskell based on embedded domain-specific lanaguages
(EDSLs) and meta-programming. We write programs in an embedded
language that is translated into efficient Haskell code at
compile-time.

The thesis also presents a case study to demonstrate the viability of
the method. The case study is a library for parallell array
computations called meta-repa. The library is based on the Haskell
library repa.
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
          | I Int
          | If Expr Expr Expr
          | Plus Expr Expr
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
  Plus :: Expr Int -> Expr Int -> Expr Int
~~~

The type variable `a` here encodes the result type of the expression;
`Expr Int` evaluates to an `Int`, `Expr Bool` evaluates to a `Bool`.

Now that the result type of the expression is encoded in the type of
the expression we can write an evaluation function with the type `Expr
a -> a`. No need to check either the well-typedness or the result type
of expression at runtime.

~~~
eval :: Expr a -> a
eval (B b) = b
eval (I i) = i
eval (If a b c) = if eval a then eval b else eval c
eval (Plus a b) = eval a + eval b
~~~

Here we see that the fact that we can give constructors custom types
means that types can depend on what constructor we have pattern
matched. In each equation of the function the return type depends on
the constructor that is being matched. For example, in the equation
for the `B` constructor the type checker knows from the type of `B`
that the return type is `Bool` so we can safely return a `Bool`.

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

## Fusion

Fusion is a optimization where temporary data structures are removed
by combining traversals. To demonstrate what this means and why we
want to do it we will look at an example. The example uses lists
rather than arrays since lists will be more familiar to Haskell
programmers. The same points apply to arrays and other data
structures.

~~~
map (*2) (map (+1) [1..100])
~~~

The naive way to evaluate this code is to first construct a list
containg the numbers from 1 to 100, construct another list with 1
added to each element, then a third list with each element multiplied
by 2. So three different lists are constructed of which only one is
actually used.  This is obviously very wasteful. What we would like to
do is to combine all these computations into one pass that just
constructs one list with the elements `(n+1)*2` for n 1 to
100. We could achive that by defining the list like this:

~~~
l = f 1
  where f n | n > 100   = []
            | otherwise = ((n+1)*2) : f (n-1)
~~~

But this is considerably less concise and readable. We would like to
be able to write programs using reusable functions like `map` and
`zipWith` while still avoiding unnecessary intermediate lists. Removing
unnecessary temporary data structures is called in this way is called
fusion or deforestation.

\TODO{background on deforestation?}

In meta-repa fusion is not an optimization that is performed as a
program transformation. Rather, the array representations and
combinators in the library are defined so that no unnecessary
intermediate arrays are created to begin with.  For example, Pull
arrays are represented as a function from an index to a value.
Combinators on Pull arrays combine the indexing functions and other
function. For example the `map` function on Pull arrays combines the
indexing function of the input with the function to map with: 

~~~
map f (Pull ixf n) = Pull (f . ixf) n
~~~

So we build up the indexing function bit by bit with the Pull array
combinators and then we do the actual traversal of the array when it
is needed. When writing the the array to memory for example.

\TODO{Example}
~~~

~~~

Sometimes the programmer wants to prevent fusion if, for example,
elements in the array are accessed multiple times, since that would
cause those elements to be computed multiple times. This can be done
with the `force` function, which writes a delayed array to memory. 

## Template Haskell

Template Haskell is a compiler extension that allows template
metaprogramming in Haskell. The programmer writes a program that
is run at compile-time and generates a Haskell AST which can be
inserted (spliced) into a Haskell program.

The meta-repa core language is translated into Haskell using Template
Haskell...




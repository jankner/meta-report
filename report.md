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
that the return type is `Bool` so we can safely return a `Bool`. The
fact that the return value of `eval` isn't tagged also simplifies it's
implementation since it doesn't have to do any checks when it calls
itself to see that the result has the right type.

As is often the case when we get more type-safety certain programs are
somewhat more difficult to write. For example, if we want to parse a
string into an expression of our language we have to first parse it
into something like our first representation and then make a function
which type checks the expression and generates the type-safe
representation of the expression if the type checking succeeds.

## Higher-order abstract syntax

Higher-order abstract syntax is a technique for representing bindings
in the object language using function values in the abstract syntax.
By doing this we reuse the notion of binding, variables and
substitution of the meta-language instead of re-implementing them. 

As a motivating example we will see what happens when we try to extend
the language in the previous section with lambda calculus constructs
to make a simply typed lambda calculus. We start by adding constructs
for abstractions, application and variables to the untyped
representation:

~~~
          | Var Name
          | App Expr Expr
          | Abs Name Expr
~~~

This is quite straight-forward, although when writing the `eval`
function we need to implement substitution or keep an environment to
keep track of variables.

Things get more troublesome when we try to do the same thing in the
GADT representation. The reason is that is difficult to give a type to
variables since there is no explicit connection between the variable's
binding site and it's uses. They are only connected by the name of the
variable. One way to solve this is to use a type-level list to keep
track of the types of variables in the environment. Variables are
represented using De Bruijn indices encoded as Peano numbers which are
used to index into the environment. \TODO{reference}

But there is a simpler solution to these problems: higher-order
abstract syntax (HOAS). Using HOAS to add lambda calculus to the typed
representation from the previous section looks like this:

~~~
  App :: Expr (a -> b) -> Expr a -> Expr b
  Abs :: (Expr a -> Expr b) -> Expr (a -> b)
  Val :: a -> Expr a
~~~

Abstractions are represented by a function in the meta-language.
Instead of having a variable constructor we use the variables of the
meta-language. For example the expression `\x -> x + 10` is
represented like this:

~~~
Abs (\x -> Plus x (I 10))
~~~

In short lambda abstractions are represented with a function that
takes an expression as argument and returns the body of the
abstraction with the variable substituted by the argument.

The `eval` function is quite straight-forward:

~~~
eval (App f a) = (eval f) (eval a)
eval (Abs f) = \x -> eval (f (Val x))
eval (Val x) = x
~~~

The `Val` constructor is only used during evaluation so we have a way to
substitute a value into an abstraction.


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
Unboxed tuples have all the restrictions that unboxed types have. In
addition unboxed tuples cannot be bound to a variable and it cannot be
passed as an argument to a function. They can essentially only be used
to return multiple values.

For performance reasons we want to use unboxed values for all
primitive types and tuples in the meta-repa code generator. For this
reason the semantics of the core language is strict since unboxed
values are strict. Also, terms in the language are monomorphic. Note
that we can still write polymorphic code generators in Haskell, so we
can still write polymorpic with meta-repa. This is similar to how
templates work in C++; the polymorpism only exists at compile-time.


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

Deforestation was first introduced in [@wadler1990deforest]. The
deforestation algorithm described in the paper is able to remove all
intermediate structures, with the restriction that the program is in
so-called *treeless form*. This is quite a restrictive form, so the
algorithm has not had great use in practice. A different approach was
used in [@gill93ashort]. The transformation isn't guaranteed to remove
*all* intermediate structure, but it is much simpler and doesn't put
any restriction on the programs it can take as input. The approach is
to have a single, simple and local rule for rewriting the combination
of two pre-define functions: `build` and `foldr`. If lists are
produced with `build` and consumed with `foldr` that one
transformation is enough to remove all intermediate lists, though of
course not all programs can be written in this way.

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

Template Haskell is a compiler extension that allows compile-time
meta-programming in Haskell. That is, it is used to write Haskell code
that generates Haskell code in the form of an AST. The generated
Haskell code can be inserted into a program at compile-time. This is
called splicing and is written `$( ... )`, where `...` is a Haskell
expression that generates an AST. The expression is evaluated by the
compiler while it is compiling the module. Because it is evaluated at
compile-time any functions used in the expression must be defined in
a seperate module, so that it can be compiled before the current
module. Splices can be occur in place of an expression, a type or
a list of top-level declarations.

The meta-repa core language is translated into Haskell using Template
Haskell and inserted using splices.

A splice can be said to allow the programmer to go from abstract
syntax to concrete syntax. Template Haskell also has quasi-quotation
which allows the programmer to go from concrete syntax to abstract
syntax. Quasi-quotation is written as `[| ... |]` where `...` is
a Haskell expression (in concrete syntax,) and the result is an AST of
that expression. There is also quasi-quotations for declarations,
types and patterns.

Template Haskell also allows programmer-defined quasi-quoters.
A programmer-defined quasi-quoter is used by writing `[quoter| ... |]`
where `quoter` is the name of the quasi-quoter. The quasi-quoter is
a collection of functions that parses a string and generates a Haskell
AST, which is inserted in place of the quasi-quotation. This allows
for programmer-defined concrete syntax, which can make programs easier
to read and write. [@mainland2007quoted] Quasi-quotation is used in
meta-repa to specify stencil computations.


# References


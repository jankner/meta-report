% Meta-repa
% Johan Ankner
%

# Introduction

Doing high-performance programming in Haskell can be somewhat painful.
Lazy evaluation and boxed representations are the default, and the
overhead associated with this can be disastrous for performance in
many cases. The programmer has to take steps to avoid laziness and to
make sure that inlining of certain functions is performed in order to
trigger certain optimization in GHC. This can be easy to get wrong,
and can give rise to performance bugs that are hard to find and fix.

This thesis presents a new method for writing high-performance
programs in Haskell based on embedded domain-specific lanaguages
(EDSLs) and meta-programming. Programs are written in an embedded
language that is translated into Haskell code at compile-time. The
code generation is done in a way to give efficiency guarantees for
programs written in the EDSL.

The thesis also presents a case study to demonstrate the viability of
the method. The case study is a library for parallell array
computations called meta-repa. The library is based on the Haskell
library repa.

This master's thesis has an unusual structure. The main content is
a research paper that was published in Haskell Symposium 2013, and
was co-written with my supervisor Josef Svenningsson. The first two
chapters are devoted to explaining some basic concepts that will be
used in the paper. The third chapter describes some features of
meta-repa that were not included in the paper. The paper itself makes
up appendix A.

## Embedded Domain-Specific Languages

A Domain-Specific Language (DSL) is a language that is focused on
solving problems in particular domain. Some examples of well-known
DSLs are: SQL for relational database queries, VHDL and Verilog for
hardware design, TeX for for typesetting.

Among other benefits using a DSL allows for domain-specific
abstractions, domain-specific optimization and having semantics that
are suited to the domain. For these reasons, among others, DSLs can
often be more concise, easier to write and understand, and more
efficient in their domain than a general purpose language. 

An Embedded DSL (EDSL) is a DSL that is embedded into a general
purpose language as a library. This saves us the work of designinging
and implementing an entire language; we borrow much of the
functionality (syntax, type system, etc.) from the host language.
An EDSL can be divided into four sorts of constituent:

* a set of types to represent concepts of the domain.
* constructor function to create elements of these types.
* combinator functions to combine and modify elements.
* run functions to make obeservations on elements.

### Deep vs. shallow embedding

There are two major techniques for embedding languages; shallow
embedding and deep embedding. In a shallow embedding elements are
represented by their semantics, the observations that can be done on
them. In a deep embedding the elements are represented by their
syntax, how they are constructed.

To demonstrate the difference between deep and shallow embedding we
will look at a simple EDSL from [@carlson1993experiment] for defining
regions on a plane, and show how it can be implemented as either
a deep or shallow embedding. The EDSL has the following interface:

~~~
type Point = (Float,Float)

inRegion :: Point -> Region -> Bool
circle :: Float -> Region
outside :: Region -> Region
(\/) :: Region -> Region -> Region
(/\) :: Region -> Region -> Region
~~~

The EDSL contains two types: `Point` and `Region`. `Region` has been
left abstract for now. It as one constructor: `circle`; three
combinators `outside`, `\/` and `/\`; and one run-function:
`inRegion`. First we will look at what the shallow embedding looks
like:

~~~
newtype Region = Region (Point -> Bool)

circle r = Region (\p -> magnitude p <= r)
outside (Region f) = Region (not . f)
(Region f1) \/ (Region f2) = Region (\p -> f1 p || f2 p)
(Region f1) /\ (Region f2) = Region (\p -> f1 p && f2 p)

inRegion p (Region f) = f p
~~~

A region is simply represented by a function that takes a point and
tells you if it's inside the region. In other words, it is represented
directly by the observation we want to do on it. Most of the work is
done in the combinators and constructors which build up the function,
and the run function simply calls the function. Next we will look at
what a deep embedding looks like:

~~~
data Region 
  = Circle Float
  | Outside Region
  | Union Region Region
  | Intersect Region Region

circle = Circle
outside = Outside
(\/) = Union
(/\) = Intersect

inRegion p (Circle r) = magnitude p <= r
inRegion p (Outside r) = not (inRegion p r)
inRegion p (Union r1 r2) = inRegion r1 || inRegion r2
inRegion p (Intersect r1 r2) = inRegion r1 && inRegion r2
~~~

The `Region` type now has a case for each of the constructors and
combinators on the language. It is represented by how the region was
constructed. The work of determining if a point is in a region is done
in the run function.

Shallow embeddings are simpler and more efficient than deep
embeddings, so it's a good choice if there is an obvious semantics for
the language. The advantage to using a deep embedding is that it's
easier to add run functions to and language and to do optimizations or
other transformation.

meta-repa uses both shallow and deep embedding. It has a core language
that is deeply embedded. The arrays that users of the library use are a
shallow embedding that is built on top of the core language. This is
described in detail in \ref{sec:shallow}.

## High-performance programming in Haskell

Let us look closer at high-performance programming in Haskell and some
of its pitfalls. As an example, let's write a function for computing
the sum of a list of numbers and use it to add a million integers:

~~~
sum [] = 0
sum (x:xs) = x + sum xs

main = print (sum [1..1000000])
~~~

When we run this program we get the following output:

~~~
Stack space overflow: current size 8388608 bytes.
~~~

We get stack overflow because each recursive call of `sum` requires
a stack frame to be allocated. A stack frame is required for the
function to remember its state when it calls itself (or another
function). But if calling itself is that last thing the function does
it does not need to keep track of anything since there nothing left to
do. A function like this is said to be tail recursive. We can rewrite
`sum` to be tail recursive like this:

~~~
sum = sum' 0
  where sum' s []     = s
        sum' s (x:xs) = sum' (s+x) xs
~~~

When we run our program with the new `sum` function we find that we
still encounter the same problem:

~~~
Stack space overflow: current size 8388608 bytes.
~~~

The problem is that Haskell is a lazy language, which means that
arguments are not evaluated before they are passed to a function; they
are only evaluated when they are needed. So the first argument of `sum'`
accumulates a growing computation as the list is consumed. When the
value of the sum is needed the entire computation is performed. For
example, evaluating the expression `sum [1,2,3,4]` will look like
this:

~~~
sum [1,2,3,4]
=> sum' 0 [1,2,3,4]
=> sum' (0+1) [2,3,4]
=> sum' ((0+1)+2) [3,4]
=> sum' (((0+1)+2)+3) [4]
=> sum' ((((0+1)+2)+3)+4) []
=> ((((0+1)+2)+3)+4)
=> (((1+2)+3)+4)
=> ((3+3)+4)
=> (6+4)
=> 10
~~~

Besides causing stack overflow for large lists building up this
computation and then immediately performing it is quite inefficient. To
prevent this we want to make the accumulator strict. We can do this by
using a bang pattern like this:

~~~
sum = sum' 0
  where sum' !s []     = s
        sum' !s (x:xs) = sum (s+x) xs
~~~

Now when we run our program we get the expected result:

~~~
500000500000
~~~

This is, of course, a very simple example and the performance problems
are quite easily solved. Also, in such a simple case GHC's strictness
analyzer can see that the result of `sum'` is consumed immediately and
automatically make the accumulator strict. But the strictness analysis
is not guaranteed to work for every program, and for more complicated
programs it gets easier to make mistakes. In meta-repa this kind of
problem is avoided by making the core language strict. Laziness is
a very useful feature of Haskell in general, but for the domain
meta-repa is tailored to strictness is a better default. In general,
with our EDSL approach we can give the programmer a set of efficiency
guarantees that are easy to understand (see \ref{sec:contract} for
meta-repa's guarantees).

# Theory


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
fact that the return value of `eval` isn't tagged also simplifies its
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
in the embedded language using function values in the abstract syntax.
By doing this we reuse the notion of binding, variables and
substitution of the host language instead of re-implementing them.
[@pfenning1988higher]

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
binding site and its uses. They are only connected by the name of the
variable. There is a solution to this, but it requires some type-level
programming and is relatively complicated. [@benton2012strongly]

But there is a simpler solution to these problems: higher-order
abstract syntax (HOAS). Using HOAS to add lambda calculus to the typed
representation from the previous section looks like this:

~~~
  App :: Expr (a -> b) -> Expr a -> Expr b
  Abs :: (Expr a -> Expr b) -> Expr (a -> b)
  Val :: a -> Expr a
~~~

Abstractions are represented by a function in the host language.
Instead of having a variable constructor we use the variables of the
host language. For example the expression `\x -> x + 10` is
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
is used to support laziness. 

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
values are strict. Also, terms in the language are monomorphic;
unboxed values do not support polymorphism. Note that we can still
write polymorphic code generators in Haskell, so we can still write
polymorpic with meta-repa. This is similar to how templates work in
C++; the polymorpism only exists at compile-time.

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
so-called *treeless form*. This is quite a restrictive form, and the
algorithm is fairly complicated to implement and has some other
problems, so it has not seen great use in practice. A different
approach was used in [@gill93ashort]. The transformation isn't
guaranteed to remove *all* intermediate structure, but it is much
simpler and doesn't put any restriction on the programs it can take as
input. The approach is to have a single, simple and local rule for
rewriting the combination of two pre-define functions: `build` and
`foldr`. If lists are produced with `build` and consumed with `foldr`
that one transformation is enough to remove all intermediate lists,
though of course not all programs can be written in this way.

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
is needed. When writing the array to memory for example.

To see how this works, consider a program to calculate the scalar
product of two arrays, `arr1` and `arr2`. The arrays are defined as
`Pull ixf1 n1` and `Pull ixf2 n2` respectively. `scalarProd`, `zipWith` and
`sumS` is defined as follows is defined as:

~~~
scalarProd a b = sumS (zipWith (*) a b)

zipWith f (Pull ixf1 n1) (Pull ixf2 n2) =
  Pull (\i -> ixf1 i `f` ixf2 i) (min n1 n2)

sumS (Pull ixf n) = forLoop n (\i s -> s + ixf i)
~~~

Conceptually `zipWith` produces a temporary array which is then
consumed by `sumS`. But as we will see this temporary array is reduced
away through normal Haskell evaluation when the abstract syntax tree
is generated. For the sake of simplicity of this example Pull arrays
are one-dimensional rather than shape polymorphic, and `forLoop` has
been treated as if it is a primitive of the core-language which it
is not in meta-repa.

~~~
   scalarProd arr1 arr2
=> sumS (zipWith (*) arr1 arr2)
=> sumS (zipwith (*) (Pull ixf1 n1) (Pull ixf2 n2)
=> sumS (Pull (\i -> ixf1 i * ixf2 i) (min n1 n2))
=> forLoop (min n1 n2) 0 (\i s -> s + ixf1 i * ixf2 i)
~~~

First the two input arrays disappear and is combined into one array,
and in the last step no arrays remain and we simply get a loop that
computes the scalar product. 

Sometimes the programmer wants to prevent fusion if, for example,
elements in the array are accessed multiple times, since that would
cause those elements to be computed multiple times. This can be done
with the `force` function, which writes a delayed array to memory. 

## Template Haskell
\label{sec:th}

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
Haskell and inserted using splices. For example, if we have
a meta-repa program in one module:

~~~
f :: Expr Int -> Expr Int
f n = sumS (enumFromTo 1 n)
~~~

we can translate this program and splice the result into another
module like this:

~~~
sumN :: Int -> Int
sumN = $( translate f )
~~~

A splice can be said to allow the programmer to go from abstract
syntax to concrete syntax. Template Haskell also has quasi-quotation
which allows the programmer to go from concrete syntax to abstract
syntax. Quasi-quotation is written as `[| ... |]` where `...` is
a Haskell expression (in concrete syntax), and the result is an AST of
that expression. There is also quasi-quotations for declarations,
types and patterns. Quasi-quotations and splices can be nested arbitrarily
deep; a quasi-quotation can have a splice inside it, which can have
a quasi-quotation inside it, and so on.

Template Haskell also allows programmer-defined quasi-quoters.
A programmer-defined quasi-quoter is used by writing `[quoter| ... |]`
where `quoter` is the name of the quasi-quoter. The quasi-quoter is
a collection of functions that parses a string and generates a Haskell
AST, which is inserted in place of the quasi-quotation. This allows
for programmer-defined concrete syntax, which can make programs easier
to read and write. [@mainland2007quoted] Quasi-quotation is used in
meta-repa to specify stencil computations.


# Stuff
\TODO{ think of a proper title}

## Manifest arrays

Manifest arrays are on of the array types in meta-repa. They are not
described in the article, so I will describe them briefly here.
Manifest arrays represent arrays that are stored in memory, unlike
Push and Pull arrays which represent computations of arrays. This
means that the computation of the Manifest array is not fused with
any computation that done with it after.

## Compiling things
\TODO{ think of a proper title}

As shown in \ref{sec:programming}, it is simple to use meta-repa
functions work with types like `Float` and `Int` and tuples and flat
arrays since they are built into the core language. Using the
`Computable` type class types like `(Expr a, Expr b)` and `Expr a ->
Expr b` are translated into `Expr (a,b)` and `Expr (a -> b)`. The
associated type `Internal` specifies what type a `Computable` is
translated to; a `Computable` type `a` is translated into `Expr
(Internal a)`. For example, `Internal (Expr a -> Expr b)` is `a -> b`,
and `Internal (Expr a, (Expr b, Expr c))` is `(a,(b,c))`.

But what do you do if, for example, you wish to use a function that
transposes a matrix? Such a function would be natural to write as
a meta-repa function of with the type `Manifest DIM2 Float -> Manifest
DIM2 Float`.

~~~
transposeP :: Manifest DIM2 Float -> Manifest DIM2 Float
transposeP arr = ...
~~~

Like the other array types `Manifest` isn't a type in the core
language, so we cannot simply take this function and compile it. We
can wrap the function so that `Manifest DIM2 Float` is translated into
`(Expr (Vector Float), Expr Int, Expr Int)`, and this could even be
automated if we wrote a `Computable` instance to do it. But when we
compile this into Haskell we get a function with the type `(Vector
Float, Int, Int) -> (Vector Float, Int, Int)`, which is less than
ideal. It would be nice if we could also wrap it into a more
meaningful type. Repa's unboxed array type `Array U` fits well for
this job as it uses `Vector` internally. Of course we alo want to do
all this converting and wrapping automatically and hide it behind
a simple interface. `Computable` can't do the work of wrapping the
compiled program, so we will solve the problem with a new type class
called `Compilable`. The class will need one function that converts
a value of the type into a `Computable`. It will also need a function
to wrap the compiled value.

~~~
class Computable (GenTy a) => Compilable a where
  type GenTy a
  type External a
  compile :: a -> GenTy a
  reconstruct :: Proxy a -> Internal (GenTy a) -> External a
  proxyOf :: a -> Proxy a
~~~

Computable value -> `compile` -> Computable value -> `translateComputable` -> TH expression -> splice -> `reconstruct` -> External representation

The associated type `GenTy a` gives the type `a` gets converted into.
The associated type `External a` is the external Haskell type that `a`
will be compiled to. The function `compile` has a somewhat misleading
name since it doesn't actually do compilation; it only converts its
input into a form that can be compiled. `reconstruct` is used to wrap
the compiled expression into the external representation. It takes an
argument of type `Internal (GenTy a)`. `Internal` is an associated
type of `Computable`, and it is used here to translate the
`Computable`'s type into the Haskell type it will compile into. For
example, `Internal (Expr a)` = `a`, `Internal (Expr a, Expr b)`
= `(a,b)`. To explain the purpose of `proxyOf` and the first argument
to `reconstruct` we will look at how `Compilable` is used.

The function of `Compilable` are not used directly by the user of the
library; instead they use the function `compileR`, which is defined
like this:

~~~
compileR :: Compilable a => a -> Q Exp
compileR a = [| reconstruct p $(translateComputable (compile a)) |]
  where p = proxyOf a
~~~

As was explained in \ref{sec:th} `[| ... |]` is a quasi-quotation
which is used here to generate a Template Haskell AST for an
expression. First input is converted into a `Computable` which is
passed to `translateComputable`, which does the actual compilation to
a Template Haskell expression. This is spliced into the
quasi-quotation as the second argument to `reconstruct`. The type of
the spliced expression will be `Internal (GenTy a)`. `Internal` and
`GenTy` are associated types which are not necessarily injective, so
we can't know what `a` was if we only have `Internal (GenTy a)`, and
there is no way to refer to the type variable `a` inside the
quasi-quotation. This leads to a problem: the compiler doesn't know
what instance to use for `reconstruct`. That is what the first
argument is for. `Proxy` is a GADT which completely reifies the type
of a `Compilable`. The reason that we pass a `Proxy a` and not simply
an `a` is that we have to lift the argument inside the
quasi-quotation, which requires an instance of `Lift` for `a`. It's
easier to write one instance of `Lift` for `Proxy` rather that one for
every `Compilable` type. `Proxy` has one constructor for every
`Compilable` instance declaration.

To demonstrate how this works, we will look at two example instances
of `Compilable`. For simplicity we will specialize the instances for
2-dimensional arrays. In the following code names imported from Repa
use the qualifier `R`. For example DIM2 is a rank-2 shape from
meta-repa, R.DIM2 is a rank-2 shape from Repa. For these
examples we assume the `Proxy` type has these two constructors:

~~~
  PManifest2 :: CProxy a -> PManifest2 (Manifest DIM2 a)
  PManifestArg2 :: CProxy a -> Proxy r
                -> PManifest2 (Manifest DIM2 a -> r)
~~~

`CProxy` is a GADT that describes the type of a `Computable`. That is,
it is to `Computable` as `Proxy` is to `Compilable`. Values of
`CProxy` are acquired with `cProxy`, which is part of the `Computable`
class.


~~~
instance (Computable a ,Storable (Internal a)) =>
    Compilable (Manifest DIM2 a)
  where
    type GenTy a =
        (Expr (Vector (Internal a)), Expr Int, Expr Int)
    type External a = R.Array R.U R.DIM2 (Internal a)
    compile (Manifest vec (Z :. y :. x)) = (vec, y, x)
    reconstruct _ (vec, y, x) =
        R.fromUnboxed (R.Z R.:. y R.:. x) vec
    proxyOf _ = PManifest2 cProxy
~~~


First, to represent the Manifest array and its dimensions in core
language types we simply use a tuple of one vector and two `Int`s.
The Haskell type we use to represent a manifest array with is the
unboxed array representation from Repa. `compile` and `reconstruct`
are straightforward. `reconstruct` doesn't need to inspect its first
argument at all, it only has to have the correct type. It constructs
the Repa array from the vector and the dimensions using `fromUnboxed`.

This let's us compile a `Manifest DIM2 a`, but it doesn't let us
compile functions, like `transposeP`. For this we need a second
instance for functions with `Manifest DIM2 a` as its argument and any
`Compilable` as return type.

~~~
instance (Computable a, Storable (Internal a), Compilable r)
    => Compilable (Manifest DIM2 a -> r)
  where
    type GenTy a =
        Expr (Vector (Internal a)) ->
        Expr Int ->
        Expr Int ->
        GenTy r
    type External a =
        R.Array R.U R.DIM2 (Internal a) -> External r
    compile f = \vec y x ->
        compile (f (Manifest vec (Z :. y :. x)))
    reconstruct (PManifestArg2 _ p) f = \uarr ->
        let arr                 = R.toUnboxed uarr
            (R.Z R.:. y R.:. x) = R.extent uarr
        in reconstruct p (f arr y x)
    proxyOf _ =
        PManifestArg2
          cProxy
          (proxyOf (error "proxyOf evaluated its argument"))
~~~

We can pass a dummy argument to `proxyOf` since it shouldn't need to
evaluate its argument.

With these two instances we can compile functions of 2-dimensional
Manifest arrays of any arity. Meta-repa has instances for `Expr`s,
Manifest arrays and Pull arrays (which are represented externally
using Repa's Delayed array representation).



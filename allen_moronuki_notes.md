2019-04-01

## Exercise: Apply Yourself

1.

The general function is:

     (++) :: [a] -> [a] -> [a]
     
Now we use it.

    myConcat x = x ++ " yo"

What is the type of myConcat?

Here, the type is

    myConcat :: [Char] -> [Char]

Check in gchi: CORRECT

2.  The general function is

    (*) :: Num a => a -> a -> a

The partially applied version is

    myMult x = (x / 3) * 5

Now what is the type of myMult?

The naive answer is:

    myMult :: Num a => a -> a

However it's possible that it'll have to be Fractional a => a, or something?

Check in ghci:

    myMult :: Fractional a => a -> a

Which makes sense, because `/` is only defined on types that have an instance of
`Fractional`.

3.  General:

    take :: Int -> [a] -> [a]

Partially-applied:

    myTake x = take x "Hey you"

The function has the second type variable bound.

So the type will be more specific.

And the type will be

    myTake :: Int -> [Char]

Check in ghci: CORRECT

4.  General:

    (>) :: Ord a => a -> a -> Bool

Partially-applied:

    myCom x = x > (length [1..10])

What is the type?

The second argument is fixed to a specific type, which will be Int.
Both arguments to `(>)` are constrainted to have that type.
They must have the same concrete type.

So it should be

   myCom :: Int -> Bool

The other possibility is 

   myCom :: Ord a => a -> Bool

But I think this is too wide.

Check in ghci: CORRECT, it was the concrete type.

5.  General function:
    
    (<) :: Ord a => a -> a -> Bool

Partially applied version:

    myAlph x = x < 'z'

Here we know that argument 2 is a `Char`.  Therefore argument 1 must also be a
`Char`.  The type class is removed.

So,

   myAlph :: Char -> Bool

Check in ghci: CORRECT.

This is kind of interesting because it means that inferred types don't
necessarily allow polymorphism in the Java-esque sense, unless you use two type
variables explicitly.

## Type inference

It's really obvious that `ourId x = x` will be inferred as `ourId :: a -> a`.
That parametrically polymorphic type is the most general type it can take.

If we do `x ++ "Julie"`, the compiler knows that the expression `x` can only be
valid if `x` is also of type `[Char]`.  Because you cannot concat heterogeneous
types.

However, if we do `x ++ y`, all we know is that we work on `[a]`.
This is plain from the type of `++`.  Actually, in this case `myGreet2` is
identical to `++`.
Remember that `++` is not the same as `concat`.


I find it useful to parenthesize the type class constraints even when they
are not product types, eg:

    f :: (Num a) => a -> a -> a

Basically `=>` should be read as a delimiting character, like semicolon or
a single colon.

If you take the type of `f 1`, you remove one argument, so you get the curried
version.  Which makes total sense.

2019-03-26

An expression like this won't work.

    6 / length [1, 2, 3]

Because `length` has the concrete result type `int`.

You can't cast the result to Fractional; it's already been narrowed.  My guess
is that we'll need to use some sort of function to convert and not just a compiler
hint.

Yes, we use a coercion function.  Such a function is named `fromIntegral`.

We can see its type.

    fromIntegral :: (Num b, Integral a) => a -> b

The interesting part is that Fractional doesn't appear in this at all.  Basically
our end procedure will be doing the following type massage:

    Int -> (Num a => a) -> (Fractional a => a)



## Exercises: Parametricity

1.  Try to write another version of id.

Options that do IO don't work, as they would in Clojure.  For instance this
attempt fails:

    davesId :: a -> a
    davesId x = do
      putStrLn $ greeting ++ ", world"
      x
      where
        greeting = "Hello"

The value of the 'do' block, although it attempt to yield x as its last
expression, actually is inferred as the type `IO b0`.

However, other syntactically equivalent variations will work, like:

    davesId :: a -> a
    davesId x = do
      x
      where
        greeting = "Hello"

So it's actually about the type of the final expression.

2.  Logically there can be only two implementations.

    davesMysteryFunction1 :: a -> a -> a
    davesMysteryFunction1 x y = x

and

    davesMysteryFunction2 :: a -> a -> a
    davesMysteryFunction2 x y = y

Or some trivial syntactic variation thereof.

3.  The signature indicates a two-argument parametrically polymorphic function.
It should only have one implementation.  And that is:

    davesPuzzle :: a -> b -> b
    davesPuzzle x y = y

2019-03-18

## Sectioning

I learned some new syntax.  [1..10] can be used to construct a list of
intervening natural numbers.  Only seems to work for integers.

`elem` is a sequence search function, it's basically `member` in scheme, or
`x in y` in Python.

Hence, 
`elem 5 [1..10]` => True

So you can use infix to search also

    5 `elem` [1..10]

And you can also section `elem`, causing it to search a fixed sequence.
Example:

    isPercentage = (`elem` [1..100])

Now `isPercentage 100` => True, `isPercentage 101` => False.
We wouldn't be able to do this with just regular currying.


## Exercise: Type arguments

1.  The answer is a) `Char -> Char -> Char`, because the type variable `a` has
been fixed at the concrete type `Char`, and one argument has been applied.

2.  The answer is d), `Char`, because all 3 'input' args have been fully applied,
and the type variable `b` was fixed to the type of the second argument, which
is `'c'`, which is of type `Char`.

3.  The answer is b) `Integer`.  It must evaluate to whatever type variable `b`
was bound to, and in this case it was bound to an Integer.  Why not `Integral b
=> b`?  This could also be possible.

WRONG

The answer is `Num b => b`.  I suppose because the compiler gives the result
the least specific type possible.

4.  Here the answer IS c) Double, because the manual hinting of the second
arg to a concrete type causes the compiler  to eagerly assign it.

CORRECT

5.  Remember that lists are a separate concept orthogonal to types, and that a
string is a list of Char.  That's why the answer from q3 doesn't apply, because
ghci treats strings specially vs numbers, there's no ambiguity as to what type
it could assign, so it just immediately assigns the concrete type.  Therefore,
the type of this in my view must be a) `[Char]`

CORRECT

6.  Well, the type should be `Eq b => b -> a`, and `a` has been inferred to a
concrete type already, so it should be `Eq b => b -> [Char]`

CORRECT

7.  It should be `Ord a => a`

WRONG

The type is actually `(Ord a, Num a) => a`.  Why???  The `Num` constraint gets
propagated onto the result type variable a for some reason.

8.  The result should be the same, `(Ord a, Num a) => a`, because only the first
argument matters to the result type.

CORRECT (at least)

10.  Here the result should be the concrete type `Integer`.

CORRECT



2019-03-17

## Manual currying and uncurrying

Uncurrying means to replace the curried arguments with tuples representing the
actual arguments.

"curry" means to convert from a function taking tuple arguments to a function
taking nested-lambda arguments.  For example, `fst` is a good example of an
already-uncurried function in this sense.  It takes one tuple argument.  `+` is
an example of a curried function.  It takes multiple nested-lambda arguments.

Using the generic uncurry allows you to call implicitly nested-lambda arguments
with tuple syntax.  eg:

    > uncurry (+) (1, 2)
    3


2019-03-11

## Exercises: Type Matching

Match the function to its type signature.

1.  not

This should have the type `Bool -> Bool`

2.  length

This should have the type `[a] -> Int` or `[a] -> Integer` but I remember that
the type was actually `[a] -> Int`.

3.  concat

This takes a list of lists and produces a list.

`[[a]] -> [a]`

I think.

4) head

As it is equivalen to `car`, this must be `[a] -> a`.

5) `<`

This would be a type class and would be something like

`Num a => a -> a -> Bool`

So my answers are:

a) `head`
b) `concat`
c) `not`
d) `length`
e)_`<`

Note that the type class that implements orderable things is `Ord`.

## Currying

All functions take one argument and return one result.

> The way the type constructor for functions -> is defined makes currying the
> default in haskell.  This is because it is an infix operator and right
> associative.

    f :: a -> a -> a

Because it's right associative, types parse like this


    f :: a -> (a -> a)

So, f is a function taking one argument, that evaluates to a function accepting
another argument.

    map :: (a -> b) -> [a] -> [b]

Here we have a type-safe map.  The first argument is a function from a -> b
establishing the parameters of the map.  It neatly transforms a list of a into
a list of b.


"Explicit parenthesization, as when an input parameter is itself a function (such
as in `map`), may be used to indicate order of evaluation, but the implicit
associativity of the function type does not mean the inner or final set of
parentheses, i.e. the result type, evaluates first."

--- WHAT?  This is strangely written but basically means that

    (a -> b)

is a quirk of the syntax for functions, and should not lead you to assume that
type variable `b` is evaluated first in any way.  That would be a weird
assumption (and incoherent?) assumption anyway.

Thoughts, how do we apply functions and use arguments that aren't the first one?

2019-03-10

After a little break, I am coming back to this.

> The compiler gives the least specific and most general type it can.

This means that directly assigning `fifteen = 15` will NOT give fifteen a
concrete class directly.

I am confused, because it's given that we can have multiple type class
constraints in a type signature, eg,

    (Num a, Num b) => a -> b -> b

But surely this would be identical to;

    Num a => a -> a -> a

Actually no, this is because I'm reading the type class constraints wrong.
The => rather serves as a separator between the type class constraints for
variables in the whole signature, and the type signature itself.
It wouldn't be the same because this signature can accept two different types
that have instances of the Num class.  The second type signature will always
need the same concrete type.

Some further notes from IRC:

    17:43        dmwit > amoe: That's a perfect reading of the type.
    17:43        dmwit > As for what implementations there could be, well...
    17:44        dmwit > About the only thing you can do with the `a` argument is throw it away.
    17:44        dmwit > But e.g. `\_ x -> 3*x+1` could be given that type.
    17:46        dmwit > If you want something that would be *inferred* to have that type, you can internally use the `a` argument before throwing it away; e.g.
    17:46        dmwit > :t \a b -> let a' = 3*a in b-1
    17:46    lambdabot > (Num a2, Num a1) => a1 -> a2 -> a2
    17:47        dmwit > N.B. a' isn't used at all in the body of the let.


2019-02-25

Reached page 127: Multiple type class constraints.

15:32         amoe > It confuses me to say that `->` is a type constructor and also an infix operator.  Because that seems to suggest that expressions at type-level work comparably to expressions at 
                     term-level.  But I don't think that this is the case.
15:34        pie__ > ive seen stuff like ((->) e)
15:34        pie__ > so maybe its true to some extent
15:36         amoe > Well, that's also confusing.  Why do I have to type `:info (->)` in ghci when, afaik, using parens to refer to infix operators is a feature of the term-level language not the type-level one
15:37         amoe > also `:t (,)` is valid but `:t (->)` is not
15:40         amoe > I think I understand the latter -- because `,` is both type-level and term-level
15:44         amoe > "Unlike the tuple constructor, though, the function type has no data constructors. The value that shows up at term level is the function. _Functions are values._"
15:45         amoe > How does it "show up"?
15:46         amoe > or perhaps it will be better for me to just accept that it somehow *does* for the moment
16:11        dmwit > amoe: Typical ways for functions to show up are via lambdas (e.g. `\x -> 3*x` is the function which accepts one argument, names it `x`, and returns the result `3*x`) or via the existence 
                     of function bindings like `f x = 3*x` (which is a function that accepts one argument, names it `x`, and returns `3*x`) after which `f` is a function value.
16:11        dmwit > It is correct to say that expressions at the type-level work comparably to expressions at term-level. Of course they are not identical, but there are many similarities.
16:12        dmwit > :i is special. It's not part of the language proper, but rather a tool offered by the compiler to let you inspect language constructs.
16:12        dmwit > Because it's compiler magic, it can do apparently magical things -- like accept both terms and types as "arguments".
16:13        dmwit > (By the way, I've recently started using "computations" and "types" to distinguish the two levels, because there are "terms" at both levels. I'm still not super pleased with this 
                     terminology, since we are moving more and more towards allowing computation in types, but I haven't thought of a clearer distinction to make yet...)

2019-02-24

The typed lambda calculus was called 'System F' by French logician Girard.
System F is a formalization of parametric polymorphism.
The `Bool` type is a set with two inhabitants.
Testing is still necessary, because runtime errors can still occur.
To avoid having the sensation that you are 'fighting with the compiler', it's
important to get into the habit of interactively type checking things in the
REPL.

If we query the type of an integer for instance, we see type class information,
because ghci will lazily assign a concrete type based on the actual usage.
You can hint the inference by saying `x = 13 :: Integer`.

The arrow `->` is a type constructor.
"The function type has no data constructors"
So you can't construct functions at the typelevel
The point is that when I say x :: a -> b  I'm actually USING the type level
constructor `->`.

    Num a => a -> a -> a

How do we read this?  The compiler actually needs the same type for a across
all arguments to the function.  However, some compiler magic will make sure
that literal values still work.  Eg in this,


   > (+) 1 2.0

ghc infers `1` as a Fractional type.


> When we query the types of numeric values, we see type class
> information instead of a concrete type, because the compiler doesn’t
> know which specific numeric type a value is until the type is either
> declared or the compiler is forced to infer a specific type based on
> the function.

This means that an expression like `2` literally has no type, it only has
a type class, `Num t => t`.  This is pretty brain exploding.

Consider

    x = 2 + 2

What's the type of this?  Some mathematics has just happened, so we'd expect
it to be Integer.  But it's actually

    > :t x
    x :: Num a => a

What did we store in x?  Not the value 4 but the expression 2 + 2, which also
has the type `Num a => a`.  This is laziness.

"Unlike the tuple constructor, though, the function type has no data constructors. The value that shows up at term level is the function. _Functions are values._"

2019-02-21

## Ch5: Types

> In haskell you cannot create untyped data so ... [except for sugar] ...
> everything originates in a data constructor.

The Bool type is a set with two inhabitants, {True,False}

> Much of what we suggest with regards to putting code in a file ... querying
> types in the REPL is about creating habits conducive to having this pleasant
> back and forth with your type system

You can query types for partially applied functions.

eg.

    Prelude> :t (+ 1)
    (+ 1) :: Num a => a -> a

Values are "in a way, fully applied functions".  This is a useful comment IMO

2019-02-18

Tuples can be heterogeneous, but lists can't.  List syntax is used both
at type level and term level, cf `[Char]` at type-level and `[1,2,3]` at term
level.  Lists can be of arbitrary size where tuples always have a fixed size.

A list of strings is denoted as [[Char]] at the typelevel.

There will later be a full chapter on lists.

`length` tells 

## Chapter Exercises for ch4

1.  The type signature of `length` as given would be:

    length :: [a] -> Integer

It takes 1 argument and returns an Integer.  Actually for some reason it's 
Integer and not Int.

2.

a) Result is `5`.  CORRECT
b) Result is `3`.  CORRECT
c) Result is `2`.  CORRECT
d) 

`concat allAwesome` will join all elements, so result will be length awesome +
length also.  So it should be `5`.  CORRECT

3.  Erm, this is confusing!
I'd say that BOTH should work.  But if anything, 6 / length [1,2,3] should fail.
It makes sense that 6 / 3 works because ghc chooses `Fractional a => a` to be
a Double by default.

Indeed this is true.
With the second we get the following.
`No instance for (Fractional Int) arising from a use of ‘/’`

4.  We could fix it using like div or mod or something:

   div 6 $ length [1,2,3]

5.  Both + and = are infix operators, however, + has a higher precedence (6)
than ==, so it evaluates as (2 + 3) == 5, so the result is True.

6.

  * `x = 5`: no value
  * `x + 3 == 5`: False


7.  My guesses are below.

    > length allAwesome == 2
    True
    > length [1, 'a', 3, 'b']
    Error: heterogeneous list
    > length allAwesome + length awesome
    5
    > (8 == 8) && ('b' < 'a')
    False
    > (8 == 8) && 9
    Error: type mismatch, 9 is not boolean

All CORRECT

8.  Palindrome done and tested in ExercisesChapter4.hs

9.  myAbs done and tested in ExercisesChapter4.hs

10.  f is a function that takes two two-tuples
It basically rearranges them.

So 

    > f (1,2) (3,4)
    ((2,4), (1,3)

This should be possible with pattern matching?  And indeed it is

## Correcting syntax

1.  The first error is about a data constructor F.  Firstly, you can't have a
function wit such a name.  There's another mistake, that they used single quotes
instead of backticks.

2.  Define it using a lambda binding as such

    myId = \x -> x

I'm not sure that we've actually seen this yet?   But either way we shouldn't
use all-caps variable names like this.

3.  This is actually the function 'fst', so it looks as such:

    myFst2 (a, b) = a

## Match the function names to their types

1.  The type of `show` should be c), `Show a => a -> String`
Note that this correct type is only visible through :t in ghci.

CORRECT

2.  The type should be b), `Eq a => a -> a -> Bool`.  The first typeclass
constraint introduces the variable a and the second is required to be identical.

CORRECT

3.  The type should be a, `(a, b) -> a` because it returns the first element
of a two-tuple.

CORRECT

4.  The type of `(+)` should be d), `Num a => a -> a -> a`.  By a process of
    elimination.  It could also be e), but I think it should be d)

CORRECT

## Remaining fun facts from Definitions

() is the zero tuple.
Parametric polymorphism is something like the function id:

    id :: a -> a
    id x = x

There are no requirements of the argument x, so it's "parametrically polymorphic".

Whereas,

    isEqual :: Eq a => a -> a -> Bool

This is *constrained polymorphism* which has requirements of its argument,
notably that it has an instance of the Eq type class.

There are 7 namespaces:

* Functions
* Term-level variables (x in `f x = x`)
* Data constructors `True`
* Type variables (`a` in `Num a => a`)
* Type constructors (`Bool`)
* Type classes (`Num`)
* Modules (`ExercisesChapter4`)

Conventions: Type variables are `a`, `b`, `c`
Functions are often called `f`, `g`, etc.
Single letter argument names are common.
Lists are often called `xs`.  This is destructured as `(x:xs)`.

2019-02-16

And is &&; or is ||.  These are proper functions.  Note that (&&) can be a
regular function in Haskell, unlike in Scheme.

## Exercise: Find the Mistakes

1.  `not True && true`

This refers to `true` which doesn't exist.  It should be `not True && True`
&& has a precedence value of 3.  This means that it should parse correctly, I
think.  Either way it evaluates to false.

2.  not (x = 6)

That doesn't work because single-equals is not an expression.  It should be 
`not (x == 5)` and yes the parens are needed.

3.  (1 * 2) > 5

This looks fine?  I don't think this has an error.  Indeed, it is correct.

4.  [Merry] > [Happy]

This should be `"Merry" > "Happy"`.  `[Merry]` would attempt to construct a list
of the value of the single binding `Merry`.  And indeed we get `Data constructor
not in scope`.  It wants to parse `Merry` as a data constructor -- presumably
because the first letter is capitalized?

5.  [1, 2, 3] ++ "look at me!"

This won't work, because it would attempt to create a heterogeneous array of
[Integer|Char].

## Conditionals

The if expression is a built-in syntactic sugar.  A given if expression has a
type.  It's even type checkable using `:t` in ghci.  Note that both the `then`
and `else` clauses must have the same type.

The if-expression must be an actual Bool type.  For instance, you can't say `if
0` or other horribly bogus things.

The indentation structure for an if expression looks like so:

foo x = 
    if expr
        then y
    else
        z
    

Note that the two expressions line up.  else lines up with if.

## Tuples

The arity of a tuple is the number of values within the tuple.  For instance,
the two-tuples is a pair (x, y).  Values in tuples do NOT need to have the same
type.

The tuple constructor is a function (,).  However it's NOT actually an infix
operator, it seems (!)  Meaning I can construct a tuple as such.

> (,) 1 2
(1,2)

This form `(1,2)` and `(1, 2)` are directly evaluable.  However the parens are
mandatory.  (Why?)

    data (,) a b = (,) a b

This is a "product type" and not a "sum type".  Recall the definition for `Bool`.

    data Bool = False | True

This `Bool` definition is a 'sum type' because it's a logical disjunction.  The
set of possible values is the sum of the legal values.  The `,` is a 'product
type' because the set of values it can take is defined by the permutations of `a`
and `b`.  (That's my guess for the reasoning.)

You can NOT have a 1-element tuple, this is actually an error, unlike in Python.

The two tuple has accessor `fst` and `snd`, this is much like `car` and `cdr`
when used with dotted pairs / improper lists in Scheme.

Data.Tuple contains accessing functions for tuples.

> The `(x, y)` syntax of the tuple is special.  The constructors you use in the
> type signatures and in your code (terms) are syntactically identical even 
> though they're different things.  Sometimes that type constructor is referred 
> to without the type variables explicitly inside of it such as `,`.  Other times,
> you'll see (a, b) -- particularly in type signatures.

The upshot of this paragraph is basically that `(a, b)` is a type-level spelling
for the data constructor `,`.  You can use that type-level spelling to
pattern-match in the term definition as well.

    davesFst :: (a, b) -> a
    davesFst (a, b) = a

    davesSnd :: (a, b) -> b
    davesSnd (a, b) = b

    > davesFst $ (,) 1 2
    1
    > davesSnd $ (,) 1 2
    2

We construct the pair using (,) and then dereference it with pattern matching.

2019-02-14

Equality operator is `==`.
Not equal is `/=`, odd.

Things that are testable for equality have an instance of typeclass Eq.
Things that can be ordered have an instance of Ord.  Much like Comparable in java.

You can't compare things that are not of the same type.

a is greater than A

Julie is greater than Chris because Chris comes before Julie.
This property falls out of the fact that String = [Char] and Char has Ord.

Won't work on our own datatype unless we add an instance of Ord to it somehow.

2019-02-12

# Chapter 4: Basic datatypes

"Thinking about types as being like sets" will guide my intuition.
Set theory was a precursor to type theory.

Logical operations like disjunction and conjunction have an equivalent in
Haskell's type system. -- UNLIKE java which does not have union types.
Actually how would conjunction work in a type system?
C's `union` actually operates more as a conjunction.

Reading or writing type signatures is referred to as the "type level" of code.
Type level is as opposed to 'term level'.  Presumably dependent types in Idris
etc. tend to blur this line.  
Term level refers to "the lines with the = expressions."

Bool is an existing type defined as `False | True`.
`False` and `True` are specific already existing data constructors.
In fact, `False :: Bool`.
My question is really, what's the TERM for `False = ?` in the prelude?
Perhaps it's a typeclass the implements `Show` or something.

`False` is called a *data constructor*.

Some type constructors may have arguments.

A *data declaration* looks as  such:

data x = y

where x is a type constructor and y is an expression-combining-data-constructors

ghci will print the definition of a type  if you do something like

`:info Bool`

It's gonna show you that Bool implements `Show` as I predicted.  It's also an
`Enum`.  It also tells me that Bool 

I could define my own Bool types.  If I did as such:

    data DavesBool = DavesFalse | DavesTrue

And then somehow implemented Show for `DavesFalse` and `DavesTrue`, although
I have no idea how I would do that.  (Update: You write `deriving Show`)

This simplifies our code and enables us to write things such as logical
inversion.

    not :: Bool -> Bool

It's quite funny to imagine trying to write this with dependent types.

It makes you curious how you would write not in haskell.  There is a really neat
way to write it.

    davesNot :: Bool -> Bool
    davesNot True = False
    davesNot False = True

In this way every function is implicitly pattern-matched.

## Exercises: Mood Swing

The data type given is as such:

    data Mood = Blah | Woot deriving Show

1.  What is the type constructor of this type?

The answer is `Mood`.

2.  If the function requires a Mood value, what are the values you could
possibly use?

The answer is `Blah` and `Woot`.

3.  You can't write this:

    changeMood :: Mood -> Woot

Woot is a data constructor naming a value, where type signatures can only name
types.  The answer would have to be `Mood -> Mood` instead.

4.  I anticipated this earlier but this is the answer:

    changeMood :: Mood -> Mood
    changeMood Woot = Blah
    changeMood Blah = Woot

This is pattern matching.  But actually we can use the underscore to denote any
value, eg `changeMood _ = Woot`.

## Numeric types


Types of numbers in haskell:

* `Int`: this is a fixed precision integer.
* `Integer`: arbitrary precision integer.
* `Word`: unsigned integer.
* `Float`: equivalent of C float.
* `Double`: equivalent of C double.
* `Rational`: this is a arbitrary precision rational number with a numerator
and denominator.
* `Fixed`: fixed-precision type that you control the length of.
* `Scientific`: A cool number type that isn't actually in the base library.

Using type classes, we make sure that operations like +, -, * operate across all
numbers, using `Num`.

**Most programs should use Integer**
Integers will OVERFLOW WITHOUT WARNING if you use ints.
You can cast data to Rational like so.  `1 / 2 :: Rational`

> Numbers are polymorphic under the surface, and the compiler doesn't assign
> them a concrete type until it is forced to.

Don't understand what's happening here:
minBound from `GHC.Int` can be used in the format:

   > minBound :: Int8
   -128

Is this a zero argument call to minBound cast to Int8?  Clearly not In fact, ::
can't even be printed.  It's not a regular operator, it's magic.  It's really
unclear what this expression means.  It looks like a type declaration.
But I don't know what it actually is.  You can coerce `Integral` values to 
an actual `Integer` using 

Be careful when using $
Because

someValue :: Integer
someValue = toInteger (minBound :: Int8)


Is not identical to

someValue :: Integer
someValue = toInteger $ minBound :: Int8

The latter doesn't parse.  So `::` has low precedence.  Division function returns
something that has an instance of the `Fractional` type class.

    > :t (/)
    (/) :: Fractional a => a -> a -> a

Why can I divide integer types???

Oh, actually the only reason is because GHC coerces its literal argument to a
more useful value.  If I do this:

davesTwo :: Integer
davesTwo = 2

And then try this
(/) davesTwo davesTwo

I get a more useful error.

    • No instance for (Fractional Integer) arising from a use of ‘/’

Which is exactly what I expect.

    16:18         amoe > In haskellbook ch4, I see that we can get the bounds of some types as such: 'minBound :: Int8'.  What's the '::' in this expression?  It seems that it's not like the operators that have 
                         been shown so far, because I can't write ':t (::)' in ghci.
    16:19       merijn > amoe: :: is just the syntax of type annotations
    16:19       merijn > amoe: Type signatures don't only work on bindings (like functions/variables) they work on expressions too
    16:21         amoe > I also saw that you could do '127 :: Int8' for instance.
    16:21       merijn > Yes
    16:21         amoe > I read that as '127 as type Int8'.  But I would read 'minBound :: Int8' as "the result of the zero argument function minBound" as Int8
    16:22         amoe > minBound = (), so () :: Int8
    16:22       merijn > amoe: minBound is not a function
    16:22         amoe > ah ok
    16:22       merijn > minBound is a value
    16:22       merijn > :t minBound
    16:22    lambdabot > Bounded a => a
    16:22       merijn > :t 1
    16:22    lambdabot > Num p => p
    16:23       merijn > amoe: Note how both are polymorphic
    16:26         amoe > I guess that because it has an instance of Bounded (if that's correct way to say it), Int8 defines minBound for itself
    16:27         amoe > and 'minBound :: Int8' will evaluate to that definition
    16:27       merijn > amoe: Right
    16:28       merijn > > minBound :: Integer
    16:28    lambdabot >  error:
    16:28    lambdabot >      • No instance for (Bounded Integer)
    16:28    lambdabot >          arising from a use of ‘minBound’
    16:29      Uniaika > > minBound Int
    16:29    lambdabot >  error:
    16:29    lambdabot >      • Data constructor not in scope: Int
    16:29    lambdabot >      • Perhaps you meant one of these:
    16:29      Uniaika > …
    16:29      Uniaika > ah.
    16:29      Uniaika > > minBound :: Int
    16:29       merijn > > minBound :: Int :)
    16:29    lambdabot >  -9223372036854775808
    16:29    lambdabot >  <hint>:1:18: error: parse error on input ‘)’
    16:30      Uniaika > :3
    16:30      Uniaika > thank merijn
    16:30         amoe > I just want to read :: as a type cast operator but it's clearly not
    16:30       merijn > amoe: Yeah, it's not
    16:30       merijn > amoe: "foo :: bar" just says "expression 'foo' has type 'bar'"
    16:31       merijn > amoe: Which GHC will then typecheck
    16:31       merijn > amoe: What is happening in the case of 1 or minBound is that the type you're telling GHC is more specific, causing it to grab the right implementation
    16:32         amoe > right, that makes sense
    16:32         amoe > thanks merijn
    16:33       merijn > The typeclass constraint of "Num a => a" and "Bounded a => a" basically says "for any type that is an instance of this class, I know how to figure out this value".

2019-02-05

# 3.7 More list functions

cons builds a list and is named `:`, it does head and tail.  Note that it's
ONLY called `:` and not called `cons`.

    Prelude> 'P' : ""
    "P"

The single char and the empty string conses together to be the string "P", as
distinct from the Char 'P'

`head` takes a list and returns the first element.  So it will return the first
char of the string.

`tail` returns NOT the last char, but all but the first char.  This is equivalent
to car/cdr in Scheme.

`take` and `drop` do as we would expect.  They don't error if they go off the
end of the string.

There is an indexing operator.  It's `!!` as infix.  This is equivalent to
nth in clojure.  Indexes are zero based.  Going off the end will cause an 
exception.  They are "unsafe".  When they are given an empty list.   Exceptions
basically suck and you shouldn't use these functions in real programs.  I'm
guessing they will later be wrapped into a monad or something.

# 3.8 Chapter Exercises

## 1 Reading syntax

Are these correct?

> concat [[1,2,3], [4,5,6]]

Concat takes a list of lists so yes.  It should return [1,2,3,4,5,6]

CORRECT

> ++ [1,2,3] [4,5,6]

Need to parenthesize to call an infix as a prefix.

Rewrite to [1,2,3] ++ [4,5,6]

CORRECT

> (++) "hello " "world"

It works and returns "hello world".

CORRECT

> ["hello" ++ "world]

Extremely wrong syntax.  Should be "hello" ++ "world"

CORRECT

> 4 !! "hello"

Type mismatch, second arg is index

CORRECT

> (!!) "hello" 4

Should work and return 'o'

CORRECT

> take "4 lovely"

Type mismatch, needs 2 args.

CORRECT

> take 3 "awesome"

Correct and returns 'awe'.

## Match the code and output

Code a), output d)
Code b), output c)
Code c), output e)
Code d), output a)
Code e), output b)

## Code exercises

Done in module.  This is just hackily hardcoding the appropriate indices using
drop and take.  We also extract out the middle part into a where variable.

A value is said to 'inhabit' a haskell type.



# 3.6 Concatenation and scoping

When you call `++` in prefix form, it can only have two arguments.

This won't work:

    secondGreeting = (++) hello " " world

Rather you need this:

    secondGreeting = (++) hello ((++) " " world)

Or equivalently

    secondGreeting = (++) hello $ (++) " " world

# 3.5 Types of concatenation functions

[[a]] -> [a]

List of lists of type a to a single list of type a.  i.e. it's flatten one
level.  Note that this is generic over the type `a`.  

`Foldable t => t [a]` can be translated to `[[a]]`.  The arrow must have a
special meaning here.

That's cool it works to flatten lists of integers too.

So BECAUSE a string is a list, you can use list functions to create strings.
You can't have lists of heterogeneously typed values.

`it` represents the last value in ghci.

The type `a` is called a _type variable_.  These type variables are called
polymorphic.

## Exercises: Syntax Errors

1.  ++ [1, 2, 3] [4, 5, 6]

This won't work because `++` is an infixr operator.
To make it work:

    (++) [1, 2, 3] [4, 5, 6]

Correct

2.  '<3' ++ ' Haskell'

Should work as is

INCORRECT -- Uses wrong quote marks. Fix is 

    "<3" ++ " Haskell"

3.  concat ["<3", " Haskell"]

Should work as is because the argument is a list of list (`[[Char]]`)
CORRECT


2019-01-29

## Ch3: Strings

Bool is a type that represents a boolean value.
Strings are lists of characters.  Char is a single character.

You can find the type of a literal value by doing

    :type 'a'

This will tell you that 'a' is a Char.

    :type "Foo"

The type here is [Char], denoting a list of Char, this is syntactic sugar, they
explain why later.  List of char is supposedly a wart sometimes addressed by
`Data.Text` and `Data.ByteString`.  You use `pack` function to get a Text.  This
is O(n) String->Text.  String is an alias for [Char] defined in `Data.String`.
This is a type alias much like `type` in Typescript.

`import qualified` will prefix a namespace similar to `as` in Python.

Char includes unicode characters.  Unicode chars are printed as '\8545', which
is octal for some reason.

Printing strings with `print` function defined in the prelude.
http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html
The search engine for Haskell is hoogle.  https://www.haskell.org/hoogle/

putStrLn will print without quotes and witha  newline.  putStr is the other one.
`print` will give the equivalent of repr() in python.

> When you enter functions directly into the REPL, ghci implicitly understand
> and implements IO without you having to specify that.

The `do` syntax allows for sequencing actions.

The 'concat' function takes a list of strings as argument and concats the
strings.

    concat ["foo", "bar"] => "foobar"

You can type where exprs also, like

    where woot :: Integer
          woot = 10

though dont know if this is necessary (no it's not)

## Exercise: Scope

1.  Yes y is in scope for z.  Because it was already defined and becomes a top
level declaration.

2.  h is not in scope for g.  Obviously, because it wasn't defined.

3.  No, there are several problems.  Pi is not defined.  d is not defined in
the formula for r.

4.  Yes

2019-01-23

Definitions: Parameter means the symbolic name for the value.  Argument is the
value itself.

## Let and Where

let introduces an expression, where is a declaration

where is bound to a forementioned declaration and is indented under it because
of this.


### Exercises: A Head Code

1.  let x = 5 in x

Should return 5.  Equivalent to (let [x 5] x)

2.  let x = 5 in x * x

Should return 5*5 eg 25.

3.  let x = 5; y = 6 in x * y

Well, I don't know if the semicolon is valid, but assuming the syntax works it
should evaluate to 30.  We learn from this that ; can be used instead of a
newline.

4.  let x = 3; y = 1000 in x + 3

This should evaluate to 3 + 3 = 6. y is unused binding

When we rewrite let to where, bindings can be out of order.  eg binding x can
refer to binding y that is defined further down.

### Exercises: 2.11

1.  2 + 2 * 3 - 1

This should parse as
2 + (2 * 3) - 1

I think.  which = 2 + 6 - 1 = 8 - 1 = 7.

Correct!

2.  (^) 10 $ 1 + 1

This should parse as:

(^) 10 (1 + 1)
= Raise 10 to the power of 2 = 100

Correct!

3.  2 ^ 2 * 4 ^ 5 + 1

Exponential is highest precedence. So this becomes

(2^2) * (4^5) + 1

= 4 * 1024 + 1
Actually, I was right about this parse.  I just worked it out wrong personally.

Equivalent expressions

1.  These will return the same result

2.  10^2 = 100
10 + 9 * 10
10 + (9 * 10)
10 + 90
100
So they will return the same.

3.  400 - 37, this is 363

(-) 37 400
This evaluates to 37 - 400, -363, so they will NOT return the same.

4.  100 div 3
This does integer division and rounds down (I think)
These won't return the same because div returns an integer, (/) returns a real.

5.  2 * 5 + 18
Parse as (2*5) + 18
= 10 + 18 
= 28

2 * (5 + 18)
2 * 23
= 46

So different.

#### More fun with functions

Rewriting stuff to evaluate in the repl.  We just rearrange the definitions
according to a topological sort based on dependencies.

z = 7
y = z + 8
x = y ^ 2
waxOn = x * 5

1.

`10 + waxOn` will evaluate to 1135.  (waxOn is 1125)
`(+10) waxOn` will evaluate to 1135.
`(-) 15 waxOn` will evaluate to -1110 or something
`(-) waxOn 15` will evluate to 1110


2.  done

3.  What does `triple waxOn` mean?  Well, it evaluates as `waxOn * 3`
Which will mean 1125 * 3 = 3375.
Worked fine.  I suppose this indicates that values aren't 'deferred' (i.e. stored
symbolically); they contain actual numbers.
    




Operator sectioning, order can matter when functions are not commutative.
Subtraction is a special case:

> (-2) 1

This won't work because (-2) == negate 2, applied to the argument 1, and you
can't apply integer to integer.

You can section though with subtraction.  (1 -) denotes a sectioned subtract.
- is an abbreviation of `subtract` function, anyway.

2019-01-22

div defines integral division
div is more useful than other integer division operations.
why does this fail?
div 20 -6
-6 parses as prefix -!


laws for quotients and remainders...

Write a function that will determine what day of the week it will be a certain number
of days after this one.  In this case mod is useful

mod (1 + 23) 7

because it's assuming to be day 1 today (Monday) and the week is 7 days long.
Mod 'wraps' the value every 7 units.  The difference between mod and rem comes
out when the arguments can be negative.

mod (-9) 7 = 5
rem (-9) 7 = -2

It seems that mostly mod is more appropriate, but perhaps rem can be useful
also.

They finally talk about negative numbers.

The simple sum `1000 + -9` will not work.
It's actually kind of good because + and - both have the same precedence so
ghc signals an error.

`1000 + (-9)`

Being able to negate numbers with `-` is syntactic sugar and nothing to do
with the `-` function.  Prefix `-` invokes the `negate` function.  And duly,
`negate 10` evaluates to `-10`.

I'm not sure we've actually seen `$` yet, but `$` is an infix operator...
Yes, they are finally going to explain `$`!

So `$` is an infix operator with the lowest possible precedence.

(2^) (2 + 2)

The parens around 2 + 2 are necessary because if you try

(2^) 2 + 2

this evaluates to 6, ie (2 ^ 2) + 2

Using $ changes this to 

(2^) $ 2 + 2

This ensures that the part AFTER the parens will be evaluated first -- because
it's low precedence AND it's defined as infixr -- and won't suck in other parts
of the expr.

Remember that 2^, +2 is a curried function.  (these are called 'operator sections' by ghc)
We can test this by assigning them to declarations in ghc.

    Prelude> x = (2^)
    Prelude> :info x
    x :: (Num a, Integral b) => b -> a

It inferred the type.

So the expr

(2^) $ (+2) $ 3*2

should be read right to left (as infixr), I think

3*2 evals first.  == 6
6 + 2 == 8
2^8 == 256

The dollar is kind of nice, I suppose.

2019-01-21

if you load files, order doesn't matter, you can declare things out of order
unlike clojure

"indentation in haskell code is significant and can change the meaning of the
code"

"whitespace is often the only mark of a function call"

"the basic rule is that code that is part of an expression should be indented 
under the beginning of that expression"

if you write a let or a do, you have to line up the let definitions.

basically the rule just seems to be that continuation expressions have some
amount of whitespace before them.
conversely, declarations must always start at column 0, or the column of the
first declaration in the module.
emacs will highlight declarations in purple

do not indent all declarations in a module!  That is bad style

1.  

    area x = 3.14 * (x * x)

the point is that floating point numbers can't contain whitespace


2.

    double x = x * 2

b doesn't exist

3. 

    x = 7
    y = 10
    f = x + y

extraneous whitespace when declaring y

2019-01-19

ghc will reply 'infixl' to an `:info` request if a function is an infix operator.
infixl means left-associative, so the leftmost operand is evaluated.
right associativity means that 2 ^ 3 ^ 4 is evaluated as 2 ^ (3^4)
The second operand gets evaluated first.
The evaluation of `2+3*4` and `(2+3)*4` are different because :info * shows that
* has a higher precedence than + and thus is evaluated first.

Exercise: Parentheses & Association
-----------------------------------


1.  a) result is 71
    b) result is 135

Correct!

2.  I believe that this would have been parsed correctly anyway.

nCorrect!

3.  f x = x / 2 + 9

/ has precedence so is evaluated as (x / 2) + 9
This would change the result of the function.

Correct!

So basically arithmetic in Haskell works exactly as you expect.

k
# environmentj


ghc
ghci
cabal

They are using ghc 8.4.3, I am using 8.0.1

Arithmetic works

Booleans are denoted by `True` and `False`

:q to quit.

There exists a package called `base` which is the base package.  ;)

ghci commands begin with :, i.e. :info

:: can be prononuced as 'has the type'

To load a file, use `:load test.hs`

    Prelude> :load test.hs
    [1 of 1] Compiling Main             ( test.hs, interpreted )
    Ok, modules loaded: Main.

Reloading will change definitions.  Once I've loaded in this "Main" module, I am
scoped to that module, and the prompt will say that.  The command :module will
'pop' the module scope in some way.  I presume it will unload the last module
although you can explicitly unload a given module with a `-` prefix.  So
`:module` is short for `:module -Main`.

Everything is an expression or a declaration.

ghci can ask for the type of a function using `:info`.

My question: what does the expression "sayHello" mean?  I must be able to pass
it to a HOF, but I can't call it with zero args.  Is `sayHello` a call or a
reference?

A "redex" is a reducible expression.

You can define functions in ghci without defining a type for them.

camelcase is used for function names

you can overwrite previous definitions

:r to reload the module

Haskell doesn't use normal evaluation, it uses "weak head normal form".

(\f -> (1, 2 + f)) 2

This expr defines a lambda that returns a pair of 1, 2+ its argument.


If I do /, it coerces to float automatically.

floats can be squared

2. 
areaOfACircle r = 3.14 * (r * r)


areaOfACircle 10 = 314

A 10m circle has an area of 314m.

3.  use prelude value

    *Main> areaOfACircle 10
    314.1592653589793


The id function exists.
Some functions are infix.
For instance, *.
All operators are functions.
Sometimes you can use regular functions in infix style.
For instance, 10 `div` 4

Wrapping + in parens will allow it to be applied in prefix form.  (+) 1 1
Names with symbols are infix by default.




















































# equivalence exercises

1. It has a free variable of z.  can't be c because that only has 1 arg

normal form is the end result of a computation / expression after all beta
reductions have been done

combinator  = lambda term with no free variables

identity function is a combinator

omega  is a diverging lambda function ie an infinite loop

((lambda (x) (x x) (lambda (x) (x x)))

this is the 'omega' infinite loop

combinators:

1. yes
2. no
3. yes
4. yes
5. no


divergence:

1. converges
2. diverges
3. converges


# 2019-09-23

## Exercises: Vehicles

1.  What is the type of `myCar`?

Its type is `Vehicle`.

2.  Define `isCar`, etc.

Do this with pattern matching -- `isCar (Car _ _) = True`
Same pattern for all types.  You must match all arguments for the data
constructor though.

Define `areCars` -- just map `isCar` over the Vehicle list.

3.  getManu

    getManu :: Vehicle -> Manufacturer
    getManu (Car x _) = x
    getManu _ = error "not a car"

This illustrates that for sum types, we only have a single type constructor to
play with.  We can't write `Car x` directly in the type signature, to avoid
having the error clause.  

4.  Error: non-exhaustive patterns.

5.  Refactor to add the size of the plane.
Create a wrapper type for Size.
Patterns for isPlane need to be adjusted.




# 2019-09-19

## Exercises: Dog Types

1.  `Doggies` is a type constructor.  CORRECT

2.  What is the kind of Doggies?

`* -> *`, as it's not fully applied.  CORRECT

3.  What is the kind of `Doggies String`?

Should be `*` I think.  CORRECT

4.  What is the type of `Husky 10`? 

Should be `Num a => Doggies a`, I think.  CORRECT

5.  What is the type of `Husky (10 :: Integer)`?

Should be `Doggies Integer`.  CORRECT

6.  What is the type of `Mastiff "Scooby Doo"`?

Should be `Doggies [Char]` I think.  CORRECT

7.  Is DogueDeBordeaux a type constructor or a data constructor?

It's both.  CORRECT

8.  What is the type of DogueDeBordeaux?

Type can only refer to the data constructor, so it's `a -> DogueDeBordeaux a`.
CORRECT.

9.  What is the type of `DogueDeBordeaux "doggie!"`

The type is `DogueDeBordeaux [Char]`.  CORRECT


## What's a type and what's data?

There is no run time type information in Haskell.  Types are erased before
runtime.
Type constructors operate at compile time.  This is the sense in which they
are "applied".
Everything after the = is a data constructor, by definition.

A&M define a Price data type.

The argument to a data constructor can be a specific TYPE; but it can't be a
specific VALUE.  You must always accept every possible value for a certain type.



# 2019-09-18

In the declaration

    data Bool = False | True

`Bool` is the 'type constructor'.
`False` and `True` are both 'data constructors'.
Type constructors are used only at the type level.
A type constructor without any arguments, eg plain `Bool`, can be referred to as
a 'type constant'.

Type constructors like 'Maybe a' are basically generic.  Maybe<A> in java.

You CAN think of `UnaryTypeCon a` as a box to put a value in.  That's how I've
been thinking of it.  But according to A&M:

> [this analogy] will betray you later -- not all type arguments to constructors
> have value-level witnesses!  Some are _phantom_.

No idea what this notion of 'phantom' means, really.

"Kinds are the types of types", oh dear.

We know that something is a fully applied, concrete type when it is represented
as `*`.  This is its kind.

When it is  `* -> *`, this means it's still waiting for another type argument.

Eg, the type-level expression `UnaryTypeCon` is `* -> *`.

You can actually examine the "kind signature" of a type constructor in ghci.
How cool is that?

    > :k Bool
    Bool :: *

This tells us that Bool is a fully 'applied' type.  A _type constant_.

Where, 

    > :k UnaryTypeCon
    UnaryTypeCon :: * -> *

This is why we call it a type 'constructor', because it's applied at typelevel
to the argument of a concrete type.

This is similar to the distinction between data constructors and constant
values.  Which are visible using :t in ghci.




# 2019-09-17

Unsatisfyingly implement maximumBy using the cheap method of destructuring.

It's possible that we can use `bool` from Data.Bool to make some of these
if statements more point free.

In the definitions, they talk a bit more abour catamorphisms, and they mention
that `bool`, `maybe`, and `either` are examples of catamorphisms.  These all
seem to be a kind of selection/branching functions.  I couldn't see any
particular pattern in their type signatures.

A&M also define tail recursion and mention that (as expected) foldr is not tail
recursive, but foldl is tail recursive.  The position of the tail call remains
syntactically visible.

# 2019-09-13

Writing many versions of many and elem, the most notable result is that
it makes sense to compose functions with different arities.

If I have some function with two args f, and a one-arg function g,

then h = (f . g)  will mean that `h 1 2` => `h (f 1) 2`.
That is, the first argument is gated through the f function.
I imagine some pretty fun stuff could happen here with respect to 'flip',
'const', etc.

# 2019-09-12

## Warm up and review

1) a) stop-vowel-stop combinations in tuples.

How many are there, dunno.

What's the type signature?

    [Char] -> [Char] -> [(Char, Char, Char)]

well how should it work?

For each stop, we want:

pap, pep, pip, pop, pup

But we also want

pak, pat, pad, pip.

So you want to fix a certain point.  For instance we could start by getting
the prefixes.

Actually the answer is clear: It's a list comprehension the classic one, this
will basically do a cubic loop.  

1b.  Just put a condition on the end of the list comprehension, and the length
of the resulting combinations list will go down to 30.


1c.  The sentences don't sound very good because they're not past tense, but
you get the idea.


2.

Well, the behaviour of `seekritFunc` depends on the definition of the `words`
Words splits on whitespace.


So what it will do is transform the list to a list of lengths of words in the
string input.

Then sum these, giving a total of word lengths.

And divide this by the number of words.
I think this gives you the mean length of a word in the string, constrained
to be an integer by the use of integer division.

To fix it to return the fractional value, use `fromIntegral` to cast.







































# 2019-09-11

Condensing the summary of the fold stuff.

## foldr

The folding function that you pass to foldr receives 'the rest of the fold',
which it can choose to invoke, as its second argument.

The function has the signature (a -> b -> b)
The first `b` here represents the rest of the fold.

Foldr works with infinite lists.  For instance, foldr const 42 [1..]
In this case the result is 1.  Why?
const returns its first argument.

    const 1 REST-OF-FOLD

at this point, REST-OF-FOLD is not evaluated and the whole thing unwinds.
Foldr should be the default choice for all reduce-type operations.

## foldl

foldl tail-calls through the list iteratively.  Its use cases are limited.

## Scans

    foldr :: (a -> b -> b) -> b -> [a] -> b
    scanr :: (a -> b -> b) -> b -> [a] -> [b]


They show a definition of scanl and show that it can be used to define the
fibonaccis as an infinite list. No idea how that works.

They introduce the "bang bang" operator.  (!!)
Type signature `[a] -> Int -> a`

They show that factorials can also be written as an infinite list.  However the
list itself does not need to be recursive, although the scanl is recursive.

As we know, fib is a tree recursion, where fac is a linear recursion.  This 
suggests that tree recursions can be notated through self-referential lists,
where basically-iterative things can be notated with simple scanl invocations.




     
































































# 2019-09-09

## Folding and evaluation

The relationship between foldl and foldr is as folllows:

    foldr f z xs = foldl (flip f) z (reverse xs)

This means that foldr is the more 'normal' fold to do your average reduction.

## Back to previous exercises...

    fixedFold5f = foldr const 'a' [1..5]

Think about it, what is this going to do?  Remember that the z applies last
in a foldl.
But also remmeber that the arguments are applied in order

FOr instance,

    foldr (/) 1 [5,2,3]

This expands as

(5 / (2 / (3 / 1)))

This means that `foldr const 'a' [1..5]`

would expand as

    (const 1 (const 2 (const 3 (const 4 (const 5 'a')))))

But that works :/
Well think about the type signature.

listOnlyFoldr :: (a -> b -> b) -> b -> [] a -> b

here we try to bind const to the first arg.

Const has to be a function of type (a -> b -> b)

That means that it has to be unified with the value of the second argument.

So it fails simply because const has the wrong type.  Not because it's
logically incoherent.

There is an IRC log that has some information on this: `foldr-const.log`


# 2019-09-06

foldl is bad because it has to evaluate the whole spine.

foldl' is better when you don't need to worry about undefined.  I'm guessing
this basically works the same as scheme's folds

## Exercises: Database Processing

Simple-ish...


# 2019-09-04

2.  Write out the evaluation steps for `foldl (flip (*)) 1 [1..3]`.

In what sense, write them out?

First, let's look at the definition of foldl.

    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl f acc [] = acc
    foldl f acc (x:xs) = foldl f (f acc x) xs

## Iteration 1

foldl (flip (*)) 1 [1..3]
foldl f acc (x:xs) = foldl f (f acc x) xs

f = (flip (*))
acc = 1
x = 1
xs = [2,3]

=> foldl f (f acc x) xs
  => (f acc x) = ((flip (*)) 1 1) = 1
=> foldl f 1 [2,3]

## Iteration 2

foldl f 1 [2,3]

f = (flip (*))
acc = 1
x = 2
xs = [3]

=> foldl f (f acc x) xs
  => (f acc x) = ((flip (*)) 1 2)
  => (*) 2 1
  => 2
=> foldl f 2 [3]

## Iteration 3

foldl f 2 [3]

f = (flip (*))
acc = 2
x = 3
xs = []

=> foldl f (f acc x) xs
  => (f acc x) = ((flip (*)) 2 3)
  => (*) 3 2
  => 6
=> foldl f 6 []

## Iteration 4

foldl f 6 []

=> foldl f acc [] = acc
=> 6

(base case has been hit)

Note that in the foldl case we strictly evaluate the function to replace the
accumulator -- it's parenthesized.

3.  One difference between foldr and foldl is:

A&M tell us above that both folds traverse the spine in the same direction, but
the difference is in the way the evaluations associate.  Associativity means the
rightmost brackets get evaluated first in the case of foldr.

So the answer is c) foldr, but not foldl, associates to the right.

4.  "Catamorphisms" are a means of deconstructing data, the roots being cata-
'down' and morphism 'shape'.

So the answer is a) Folds are generally used to *reduce structure*.

5.  

a) fixedFold5a = foldr (++) "" ["woot", "WOOT", "woot"]

It needs the identity case z that will execute last.

b)
fixedFold5b = `foldr max [] "fear is the little death"`

 Welp, a string is a list.  max is a two-argument function.
I suppose that this is designed to find the highest char in the string.
[] is not a sensible z-value.
OTOH, we have no idea what a sensible z-value would be.
I guess that it has to be a char.  So let's just pick a.

How would we write it as a regular recursion?
I wrote it like this

    maxCharInString :: [Char] -> Char
    maxCharInString [] = undefined
    maxCharInString (x:xs) = go xs x 
      where go [] val = val
            go (x:xs) val = go xs (max val x)

Which is a left fold.  That would translate as 

    fixedFold5b = foldr max 'f' "ear is the little death"

But that only expresses the `go` function not the outer `maxCharInString`
function.

c) `foldr and True [False, True]`

The `and` function already takes a list, so this looks more like an
implementation of the `and` function.  The binary function for logical-AND
is denoted as (&&).

So the answer is

    fixedFold5c = foldr (&&) True [False, True]

d) Can it ever return a different answer?  No, because of the use of || in the
zero.  It's not a proper zero as it doesn't preserve the result.  Rather the
|| True percolates all the way down the RHS of the fold.

e) `foldl ((++) . show) "" [1..5]`

What is going on here...

The result type will be a String.
It will just concatenate the stringified numbers, so the result should be
"12345".
The key is that the showable takes the arg in the wrong place.

Replace foldl with foldr to win.

I don't understand these ones that are using const.

# 2019-09-03

## Exercise: Understanding Folds

1.  

a) should not type check, as can't multiply Num a => a with a list
indeed it does not

b) flipping the args of *

in foldl, y normally represents rest of the fold
in this case x rep. rest of the fold

So it intuitively seems like it should just invert the fold direction but it
does not actually do that.  `foldl (flip (*)) 1 [1..5]` evaluates as

    (5*(4*(3*(2*(1*1)))))

BUT will yield the same outcome 

checking this with the visualizer

c) would evaluate as ((((1 * 1) * 2) * 3) * 4) * 5 = 120
checked in vizFold

So to summarize:

    regular foldr:  (1*(2*(3*(4*(5*1)))))
    flipped foldr:  (((((1*5)*4)*3)*2)*1
    regular foldl:  (((((1*1)*2)*3)*4)*5)
    flipped foldl:  (5*(4*(3*(2*(1*1)))))


# 2019-09-02

Summarizing the stuff on fold.  Fold can avoid evaluating the spine.  If fold
evaluates a bottom value as part of its operation, it will return bottom.

This is in contrast to a function like `length`, which is going to always
evaluate the entire spine.  However if you preprocess with take to remove the
bottom value like so:

   length $ take 4 ([1,2,3,4] ++ undefined)

This is still going to work (because take conses a new list I presume).

A&M introduce this expression: 

    length $ take 2 $ take 4 xs

How does this work?  It does work, which means that `take 4` never fully
evaluates.  The only way this could happen is if control & evaluation is passing
through the pipeline from the `length` invocation to the `take` invocation.

So `length` asks for one element of its continuation, `take 2` asks for 1
element of its continuation, `take 4` yields one element, repeat for a while
until `take 2` hits its limit.

Remember that the identity value `z` forms the END of the recursive call.
Hence --

      > foldr const 0 [1..5]
      1

Foldl works in a more reasonable way: the first items are evaluated first,
starting with the seed (named 'acc' in this example).

Scanr and scanl are functions that execute folds but return lists of the
intervening results (amazing)
This sounds very useful for debugging folds.

foldl moves the z to the start.

Sketch the evaluation of

    foldl (flip const) 0 [1..5]
    
Well, foldl passes the 'acc' value (0) and the argument representing the rest of
the fold to its function.
A flipped const looks like this

    flippedConst _ y = y

That means that y will be here.  But hang on, y represents the rest of the fold.
So this reduces eventually to `flippedConst 4 5` at which point the fold will
end.



# 2019-08-22

Fold right is right associative, you can see this from the recursive step in
its definition.

    myFoldr f z (x:xs) = f x (foldr f z xs)

evaluating it, z stays the same through every iteration and could actually
be eschewed.
When we hit this line --

    (x:xs) -> f x (foldr f z xs)

Whether the expression `(foldr f z xs)` is evaluated or not depends on the
strictness of the function `f`.  As `(+)` is known to be 'strict in both its
arguments', we force the whole thing.

An example would be `const` which will always return the first argument.
`const` will avoid even evaluating its second argument.  So we could pass it
into fold and that would effectively short circuit the rest of the fold.

When evaluating, calls "alternate between the folding function f and foldr".
This means that the function f receives two unevaluated pieces, x and y, and can
decide whether it wants to evaluate them.  If it evaluates y, control will pass
back to `foldr`.

A&M say that (some expression x) is 'implicitly wrapped around' (some other
expression) to denote that X is the continuation (and write 'implicitly' to
state that 'you cannot tell this from the code sample that we are about to
show').

> One way to think about the way Haskell evaluates is that it's like a text
> rewriting system.

-- and, surely, this 'rewriting' (substitution) is what referential transparency
is all about.

Traversal is the stage where "the fold recurses over the spine".
Folding itself is the reduction of the folding function.

Hence, foldr gives control of evaluation over both the spine traversal.  A
function passed in to fold can not evaluate its arguments, for instance
using (||).

The function `repeat n` will repeat its argument infinitely.

2019-08-21

'The base case is the identity for the function' ie the Y of that type
whereby OP(X, Y) = X

It's wrong to think of the value as a 'seed'  Rather it should be named a
'zero'.

Because of laziness folds can choose to exit the fold early.

# 2019-08-20

A 'product type' is a type that contains other types, a sum type is a type
that's the disjunction of several other types.  It's denoted using data
constructors with more than one argument.

A&M show an example of how to define your own cons cell, much like we did
earlier.

"The spine is a way to refer to the structure that glues a collection of values
together."
i.e. it's the nesting of cons cells but with the values elided.  This is
perfectly meaningful because of non-strict evaluation.

# CHAPTER 10 -- folds

Folds are also referred to as catamorphisms.  The prefix 'cata' means down or
against.  Catamorphisms are a way of *deconstructing data* ie a reduction of the
data.  (but they point out that folds can also return lists -- folds can build
up a new structure as a result -- would be interesting to see an example of
this???)

There are also something called scans.

foldr is given, the type signature seems to indicate that it's reduce.  And we
can see that in practice it works the same as reduce.  Haskell does not have a
function called reduce, so reduce is the same as fold-right.

Something interesting is shown here, the type signature notation `[] a` as the
more-regular-less-sugary way of writing `[a]`.  This works fine:

    myFoo :: [a] -> [] a
    myFoo xs = map id xs

So in a very real sense, the list is a regular product tye that happens to
be named `[]`.

Although foldr is replaced with a type class called Foldable, you can still
define your own version that's more concrete, by just rebinding the function
with a new type signature.

My question about the type signature is why make the type the same as the 2nd
arg of the function???
Also how does this actually work because the function f is defined to take
a value of type `a` as its first argument.  But in the recursive step, it may be
a different type.  Actually no, this is wrong, in the signature

(a -> b -> b)

b denotes the seed value.  So this will be where the direction (right or left)
comes in.  Take the case of cons

    (cons B-VAL [])
    (cons 3 (cons B-VAL []))
    (cons 2 (cons 3 (cons B-VAL [])))
    (cons 1 (cons 2 (cons 3 (cons B-VAL []))))

So as we can see the operation proceeds from the right.

The relationship between map and foldr: Map traverses the spine and essentially
replaces the values with function applications to the values.  Foldr replaces
the spine itself with function applications.  (which are then inevitably
reduced.  hence in the case where the function it replaced them with is "cons",
it still returns a list, otherwise if the function is (+), it returns a single
value.)

# 2019-08-16

The eventual way to do `reverse` is to use (++), which feels kind of cheap to
me.  But I didn't even think about using it...

myConcat just applies ++ recursively.  Hence concat is basically "apply append".
There's some deep relationship between "apply" as a concept and primitive list
recursion on 2-argument functions.

"squishmap" -- is this mapcat in clojure?


squishMap (\x -> [1, x, 3]) [2]

squishMap can easily be implemented by just composing concat and map.

myMaximumBy this is interesting.  It can be done by keeping a counter of the
max-so-far
But we probably want to do a  pattern match.  This means we need to use case?

This is nice and difficult -- it introduces new concepts:

note that you need ot have the one-element case here.
this is actually a fold/reduce
but we haven't encountered that yet so we can't use it
it also needs an accumulator/go-function and 

I eventually needed an explicit counter because how do you prevent trying to
evaluate an undefined?  I am guessing that there is a clever head recursive
version which I have overlooked.  Something that handles the first-element case
by destructuring... or you could go through the list backwards?  which would
allow you to recurse until (x:[]), then unwind

2019-08-15

## Writing stdlib functions

Writing And was pretty fun because you can abuse pattern matching, the
implementation basically being a direct translation of the thought process.


reverse needs to have a think.

I think that one way to do it is to use a tail recursion in a go function?  




2019-08-08

## Transforming lists of values

map is used with regular lists, fmap is used with something called `Functor`,
but we don't know what that is yet.

They work the same in many examples.

The type of map is:

    map :: (a -> b) -> [a] -> [b]
    fmap :: Functor f => (a -> b) -> f a -> f b

I understand the regular map but not the type of fmap.  What does 'f a' mean.

Let's take a call to `map (+1)`

In this case, `(a -> b)` resolves to `Num a => a -> a`

In that case, the type becomes

    expr :: Num a => (a -> a) -> [a] -> [a]

Now try with fmap.

    fmap (+1)

The type of `fmap (+1)` according to ghci is:

    (Functor f, Num b) => f b -> f b

A&M go into some detail about the evaluation of map, and emphasize that it's
basically threading the function into each cons cell.  However:

> crucially, map doesn't traverse the whole list and apply the function
> immediately.  The function is applied to the values you force out of the list
> one by one.

The proof of this is this expression, which was alluded to in the previous
set of exercises:

    take 2 $ map (+1) [1, 2, undefined]

If we had eager (strict) evaluation, this would blow up, because it would
attempt to evaluate the `undefined`.

## Exercises: More Bottoms

1.  Value or bottom?  `take 1 $ map (+1) [undefined, 2, 3]`

Bottom.  CORRECT

2.  Value or bottom?  `take 1 $ map (+1) [1, undefined, 3]`

Value [2].  CORRECT.

3.  Value or bottom?  `take 2 $ map (+1) [1, undefined, 3]`

Bottom after evaluating to [2,].  CORRECT

4.  The type of `itIsMystery` is

itIsMystery :: [a] -> [Bool]

WRONG -- The actual type is [Char] -> [Bool], the input x is restricted to Char
by the context of the Elem call.

5.  What will be the result of the following functions

a) A list of the squares of the numbers 1..10 inclusive.  CORRECT

b) minimum is going to be applied over the list.
It returns 'the least element of a non-empty structure'.

So, it's going to return [1, 10, 20].

c) 5+4+3+2+1 = 15, so [15, 15, 15]

6.  There is some function in Data.Bool

its type signature is `a -> a -> Bool -> a`

ie take two arguments of the same type and yield one of the values depending
on the truth value of the argument.
indeed -- if the value is false, the first value will be returned, otherwise
the second value will be returned.

The implementation looks like: 

    ex6 = map (\x -> bool x (negate x) (x == 3)) [1..5]

This is pretty unclear IMO

## Ex: Filtering

1.  Write a function to give multiples of 3 from a list 1..30.

x is a multiple of y if rem x y == 0.  Something is even if it is a multiple of
2.

Therefore,

`filterAnswer1 = filter (\x -> (rem x 3) == 0) [1..30]` is
`[3,6,9,12,15,18,21,24,27,30]`

But it needs to be a function, so do a curried version.

: filterAnswer2 = length . multiplesOfThree

3.  Write a split function to remove articles.  Now we are allowed to use
the built in Prelude function `words`.  The answer is quite simple and uses
`elem` to check membership of the set of articles.

## Zipping lists

The default zip function pairs arguments using the 2-tuple constructor.

zip only runs for the length of the shortest list.

Unzip will do the inverse operation which is its even less clear why you'd
want it, and return a tuple consisting of two lists.  But it's not a lossless
process because zip can stop on the shortest list, so it's not guaranteed that
all of the information will ever reach the unzip call.

zipWith is a more interesting function that combines data from lists in an
unspecified way.

It's equal to (imperative pseudocode)


    result = []
    max_index = min(len(l1), len(l2))
    for i in 0..max_index:
        result.append(f(l1[i], l2[i]))


## Exercises

1.  Write your own version of zip.
The key is to remember that pattern match destructuring syntax is (x:xs).

In scheme this would be something like

    (define (zip xs ys)
      (cond
        ((null? xs)  '())
        ((null? ys)  '())
        (else (cons (make-tuple (car xs) (car ys))
                    (zip (cdr xs) (cdr ys))))))
      
        
We learned that the name for the 2-tuple constructor is (,) and the name for the
3-tuple constructor is (,,).

## Chapter exercises

1.  isUpper has the type `Char -> Bool`.   toUpper has the type `Char -> Char`.

2.  We just write 'filter isUpper'.

3.  We destructure to get the first item of the list then apply toUpper.

4.  We add a base case and recurse on the cdr.

5.  A&M introduce the 'head' function.  This function is notorious for some
    reason.  My guess is that it's a partial function.  What's the head of an
    empty list?  -- it's an exception.  I think that we have actually come
    across this before when dealing with Maybe.  And writing this means that
    we write a partial function, because there's no sensible value when the list
    is empty.

6.  Written as composed and pointfree now.

## Cipher exercise

Wanted to use Enum and succ/pred which is simple but doesn't wrap around the
alphabet as it's supposed to.
My solution seems to meet the requirements but has several problems
Doesn't support capitals
Doesn't support spaces
Only supports 'a'..'z'
Apart from that it seems OK.  As promised this was quite difficult.



2019-08-07

Binding things to _ is not merely a convention.  It prevents the compiler
from attempting to evaluate whatever is in that spot.

Look at creating some weird lists:



    weirdList = [1] ++ undefined ++ [3]

You can't either a) print this list, or b) get the length of it.
Why not?
Because this is not actually consing together a list fresh: it's welding
together existing spines.
As a result, undefined appears directly in the spine, causing the `length`
function to blow up.

This is fine though: `weirdList2 = [1] ++ [undefined] ++ [3]`
This one can be got the length of, but it can't be printed.

## Exercises: Bottom Madness

1.  It will be undefined because you can't evaluate the whole sequence.  However
the first item can be evaluated.  The first one should be 1^2 = 1, then it
should blow up.  CORRECT

2.  Should be fine and result in `[1]`.  CORRECT

3.  This should fail because it has to force and reduce the whole spine+values
before it can return anything.  CORRECT

4.  This should succeed and return 3.  CORRECT

5.  This won't work because it's an attempt to cons onto a spine and length
is spine strict. 

6.  It's really difficult to tell what this one will do.  With the most lazy
interpretation, it would return [2].    The outer expression `take 1` would
cause the inner call to stop.  And indeed that is CORRECT.

The question is though, do we have to write `take` specially to avoid forcing
the values?  Probably not!

7.  Without the 2 in the list, the `take` call will keep reducing the filter
    expression, and will attempt to evaluate undefined, so it will blow up.
CORRECT

8.  This will stop after producing the initial value [1].  CORRECT

9.  This will stop after producing [1, 2].  CORRECT

10.  This will blow up.

## Intermission: Is it in normal form?

1.  `[1, 2, 3, 4, 5]`: This expression is in NF.

2.  `1 : 2 : 3 : 4 : _`: 
If _ is evaluated, it's in NF, otherwise, it's only WHNF.  Since _ (hole)
can't be evaluated by definition, it's in WHNF.

3.  `enumFromTo 1 10`: The head is enumFromTo so surely it is
in neither.

4.  Again it's in neither: the head is length.

5.  Again it's in neither: the head is sum.

6.  Again it's in neither: the head is ++.

7.  `(_, 'b')`: This is in WHNF but not NF, by definition the hole is not
    evaluated.







2019-08-06

:sprint is a command that can show to what extent some variable has been
evaluated.
If it hasn't been referenced yet, it will print as the underscore.

Except in the case of Num a => a, which will not evlauate ever as it's 'waiting
for a sort of argument' in A&M terms, so ignore it in this case.

Stuff will only get made non-_ as you take it.  Eg,

    Prelude> Prelude> l1 = enumFromTo 'a' 'z'
    Prelude> take 1 l1
    "a"
    Prelude> :sprint l1
    l1 = 'a' : _
    Prelude> take 2 l1
    "ab"
    Prelude> :sprint l1
    l1 = 'a' : 'b' : _

As we have not reached the end of the list yet, the final item is the
unevaluated cdr of a cons cell.

`length` is one of those special functions that evaluates only the spine of the
list.  Unfortunately due to optimizations, it will seem as if the whole list was
evaluated.  eg it shows `"abc"` when it should show `_ : _ : _ : []`.

There is something called weak head normal form, WHNF, which means that
expressions can be reduced to the point where they are 'waiting' for another
input, rather than completely reduced (normal form -- NF).

If we evaluate to NF, we say the expression has been fully evaluated, as far as
it will go.

Haskell actually does have the 'exploding value' that I dreamt about: the
literal `undefined`.  Evaluating `undefined` will throw an exception but only
when it's actually evaluated.  You can store undefined in a list and ask
for the length of a list without an exception being thrown, because the `length`
function in prelude is spine-strict.  It's "spine-strict" in the sense that
it forces the evaluation of the entire spine, but it's not *value-strict*
because it doesn't evaluate the values in either the car or cdr of the cons
cells constituting the spine.


2019-08-05

## Exercises: Comprehend Thy Lists

1.  mySqr is a list of squares from 1..10.
So the result will be a filtered list of even squares.
Which means that it will be `[4, 16, 36, 64, 100]`  -- CORRECT

2.  x and y are elements of mysqr.
    The result is a tuple
    x is restricted to less-than 50: [1,4,9,16,25,36,49]
    y is restricted to greater-than 50: [64, 81, 100]

So the result should be (giant long list called wanted2) -- and result is true.

3.  It's just the result of taking the first 5 so it should be

`(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)`



Now continue.

'elem' is the `in` keyword in python -- linear search for an item and return a
boolean value.
Write a list comprehension to remove all of the lowercase letters from a string.


## Spines and non strict evaluation

A&M suggest that a tree is a data structure in Haskell.
We say that a list has a spine.
What constitutes the spine -- the cons cells.
The infix notation encourages you to think that the value is primary, but
actually the cons cell is primary.  Hence perhaps why I wanted to write
cons in prefix instead.

Some confusing statements:

"It is possible to evaluate only the spine of a list without evaluating
individual values"

"Until a value is consumed, there are a series of placeholders as a blueprint
of the list that can be constructed when it's needed."


2019-08-01

## list comprehensions

these are denoted with a [<function> | <var> <- <input set>]

These seem very similar to the ones in python.

There is also a version that has a predicate.  Still very similar
the predicate is separated by a comma.

you can have multiple generators

lcVal3 = [x^y | x <- [1..10], y <- [2..3], x ^ y < 200]

This expands out to a loop like

for x in [1..10]
  for y in [2..3]


You can use tuples in generator expressions to pair things up

2019-07-29

# Exercise: EnumFromTo

The basic strategy for this is to loop and have 3 conditions, 2 base cases and
one recursion step.

take works as we expect.
drop works as we expect.

splitAt splits at an index and returns a 2-tuple
The type signature basically says what it does.

take can be used with infinite lists like the ones that result from enumFrom
calls.

takeWhile and dropWhile are weird
takeWhile is like filter.  Except that it stops immediately on the first
non-true.
eg takeWhile (>3) [1..10]  evals to []
but takeWhile (

That's actually really really useful.

## Exercise: Thy Fearful Symmetry

1.  This is a tricky exercise because you have to remove the initial space that
gets included as a result.  And you have to use this trimmed version at both
sites.  Otherwise, it's a simple build-by-recursing thing.


2.  Implemented in PoemLines.hs

3.  Implemented in Chapter9.hs as `split`



2019-07-25

In Haskell, lists are both finite sequences and also infinite streams.

: is an infix operator meaning cons.
As in lisp, cons takes a head and a taila nd returns a list.
It works in the same way, you can cons the head onto an empty list.

Hence, `(:) 1 []` => `[1]`, we constructed a list from one item.

This line noise:

    (:) 1 $ (:) 1 []

Gives a list of two items.  

When looking at the list definition,

    data [] a = [] | a : [a]

You read that as:

SUM([], PRODUCT(:, a, [a]))

Recall that a product type is an aggregate.

    data MyProductType a b = MyProductType a b

In this case, MyProductType is just a prefix function.
You can create a near identical, less magical type by the following.

    data MyList a = EmptyList | MyList a (MyList a)
      deriving (Show)

Hence cons is not really a function per se!  It's just a data constructor.
It doens't even have a definition.

Now, writing this:

    MyList "bar" (MyList "foo" EmptyList)

Is completely isomorphic to this:

    (:) "bar" $ (:) "foo" []
   
Although they have a special print syntax, which can just be defined by giving
them an instance of the Show type class.

The list in question is a singly-linked list.

## Pattern matching on lists

When we write `myHead (x : _) = x`, we define a function without a type
signature.  This looks weird but we are just matching on the data constructor,
which happens to be infix in this case.

myHead and myTail are partial functions however.  Even the prelude from head is
a partial function and as such you arguably shouldn't use it?

The comma in lists is syntactic sugar.  Does it relate to the (,) tuple operator
at all?  The ability to enclose lists in square brackets is all magic.
Otherwise you would have to use explicit cons calls.

There is something called a spine that 'is the connective structure that holds
the cons cells together and in place'.  This isn't very clear.

There is also sugar to construct list from ranges like Perl.

[1..10] expands to [1,2,3,4,5,6,7,8,9,10]
it's directly equivalent to an enumFrom
You can also use [x,y...z]
which will expand to enumFromThenTo, 
this creates sequence with a step.  It might be called enumWithStep instead.
enumFrom, enumFromThen creates infinite lists


2019-07-22

## Numbers into words

See the solution to hundredsDigit

    hundredsDigit :: Integral a => a -> a
    hundredsDigit x = d
      where (x', _) = divMod x 100
            d = mod x' 10


























2019-07-19

Implemented a dividedBy that handles all cases.  Confirmed against neallred's
solution, although I like mine better because it has less if statements.

mc91 is truly nuts, I have no idea how this was derived, although it's easy to
write in Haskell.


2019-07-18

## Fixing dividedBy

dividedBy will just loop infinitely when denom is 0

why?  Because the n value will never decrease.

In the case where denom is 2, we wnat the result to be -5
This means that it's like we should just invert the result

dividedBy' 20 0
=> go 20 0 0


2019-07-17

In haskell, these internal functions that are used to provide a counter etc
are called 'go' functions.

## Chapter 8 Exercises

1. The type of `[[True, False], [True, True], [False, True]]` is a) `[[Bool]]`
check -- CORRECT.

2.  a) Does not have the same type, because the inner type is the 2-tuple.
    b) Does have the same type, even though the lists are different lengths
    c) Does not have the same type (it's `[Bool]`)
    d) Does not have the same type (it's `[String]`)

So the answer is b).  check -- CORRECT

3.  d) All of the above are true.  The type variable [a] fixes the content of
the list.

4.  b) is a valid application: func "Hello" "World".  CORRECT

## Reviewing currying

1.  The value should be "woops mrow woohoo!".  CORRECT
2.  Flippy has fixed the last argument at haha, so the value will be "1 mrow
    haha".  CORRECT
3.  appedCatty "2" => "woops mrow 2"
    So, `frappe (appedCatty "2")` => "woops mrow 2 mrow haha".  CORRECT
4.  frappe "blue" should be "blue mrow haha".So, "woops mrow blue mrow haha".
    CORRECT
5.  This one takes a bit more working out 

cattyConny (frappe "pink")
           (cattyConny "green" (appedCatty "blue"))

Evaluate (appedCatty  "blue") => "woops mrow blue"
now eval the 2nd expr
"green mrow woops mrow blue"

=> "pink mrow hahah mrow green mrow woops mrow blue"
CORRECT
6.  "are mrow Pugs mrow awesome"
CORRECT

## Recursion

1.  First go back to the definition of dividedBy.

dividedBy 15 2
=> go 15 2 0
=> go 13 2 1
=> go 11 2 2
=> go  9 2 3
=> go  7 2 4
=> go  5 2 5
=> go  3 2 6
=> go  1 2 7

Result is (7, 1) 

2.  Several approaches but I will just do the naive one.

3.  to multiply 3x3, you want 9
    you add 3 to 3 two times

to multiply 3x4, you want 12, you add 3 to 3 3 times.

6*6, you want 36, so the result try

The solution looks similar, see iteratedMultiply

















2019-07-14

# Chapter 8: Recursion

Recursion gives a way of expressing 'indefinite' computation.  Which is
basically a way of saying you can write loops that can't be unrolled at compile
time.

It seems that the lambda calculus can't express recursion, though, because we
don't have any way to name things.  (The answer to this is the Y combinator
which is out of scope at the moment.)

'It is not often necessary to write our own recursive functions' -- you learn
this from Clojure, that HOFs basically make most uses of 'recur' obsolete.

Write a factorial function.  4! = 4*3*2*1

It's pretty simple, you just match on the 1 value, and the rest is much like in
Scheme.

As we know, recursion always needs a base case to terminate.

Look at this sentence:

> Making the base case an identity value for the function (multiplication in this
> case) means that applying the function to the case doesn't change the result
> of previous applications.

In this case 'identity value' for a function means that n*1 = n. 

Something about composition...

## Exercise

Write out the evaluation of `applyTimes 5 (+1) 5`

It should be evaluated as such.  It means apply +1 5 times to the seed 5.

applyTimes n f b

applyTimes 5 (+1) 5 = (+1) (applyTimes (5 - 1) (+1) 5)
  (+1) (+1) (applyTimes (4 - 1) (+1) 5)
  (+1) (+1) (+1) (applyTimes (3 - 1) (+1) 5)
  (+1) (+1) (+1) (+1) (applyTimes (2 - 1) (+1) 5)
  (+1) (+1) (+1) (+1) (+1) (applyTimes (1 - 1) (+1) 5)
  (+1) (+1) (+1) (+1) (+1) (applyTimes 0 (+1) 5)
  (+1) (+1) (+1) (+1) (+1) 5
  (+1) (+1) (+1) (+1) 6
  (+1) (+1) (+1) 7
  (+1) (+1) 8
  (+1) 9
  10

## Bottom

There is a value named bottom that is written as an 'up tack'.
⊥
This is used for two things: errors and infinite recursions.

    let x = x in x

This is an infinite recursion.  Another case of bottom is when a function gives
an error, either manually (using the `error` function) or due to being a partial
function.  When you don't pattern match everything, you write a partial
function.  Such a function will type check with a warning and then give an error
(a bottom type) at runtime.

They introduce `Maybe`, which is basically equivalent to optional.

You can destructure a Maybe using pattern matching.

    let (Just y) = (f2 False) in y

Maybe is only skirted over here.
















2019-07-10
----------

# Chapter definitions for Chapter 7 notes

When we talk of an 'uncurried function', we mean a function that accepts
all its arguments as a single tuple.

A product type is an aggregate value, i.e. one holding more than one atom.
A sum type is a set of values (an enum) -- like Bool, can be False | True.

eg

data MyProductType a b = MyProductType a b

MyProductType now holds two parametrically-polymorphic values.

A sum type looks like this:

    data SumOfThree a b c = FirstPossible a | SecondPossible b | ThirdPossible c

Bottom is a notional type used to indivate the lack of a value.  It can't be
written.  There is some function called error that can be used to throw
exceptions.

The `const` function takes two args and returns the first.

2019-07-04
----------

## Chapter Exercises for Chapter 7

1.  A polymorphic function...
d) may resolve to values of different types, depending on inputs.

2.  b) Char -> [String]

3.  Ord a => a -> a -> Bool

a) Ord a => a -> Bool after one argument applied.   WRONG!
Actually the answer is d)
(Ord a, Num a) => a -> Bool

Somehow it picked up the Num constraint from being applied to its argument.
I presume that if it has been a concrete type then that would have replaced 
both constraints -- AND IT DID.

4.  A function of type (a -> b) -> c

b) is a higher-order function.

5.  Given that f is the identity function, what's the type of f True.

It should be `f True :: Bool`  CORRECT

## Let's write code

1.  Digits functions:

a) Rewritten to use divMod by just discarding the second part of the tuple.
b) Yes it does.
c) Implemented a version that gets the hundreds digit by just divving with 100
instead.

2.  Implemented the case and guard versions.

3.  Implemented and working, test case is:

```
g (^2) (4, 1)
```

This is the only possible definition.

4.  Wrote a round-trip program.

5.  Wrote a point-free version.

6.  The answer is this abomination:

    print $ ((roundTrip2 4) :: Int)

All parens are needed.  This is a very confusing exercise phrasing because
it suggests that you need to adjust the definition of the function, when in
fact you need to adjust the call site.

## Function composition

Composition creates a two-step function, ie it goes straight from a->c where
we usually have to go through two stages.

There is a function `odd` that already exists; and a function `filter`, which
works as we expect

Hence `filter odd [1..10]`.

Using function composition enables point free style.

Type inference means that we can just write pointfree definitions.

foldr is fold-right, ie reduce.

The function f will count the number of occurrences of 'a' in a string.  It's
really cool that the pure use of the (== 'a') basically implies that it would
have its natural type.

Things like `(+3)` somehow work correctly, but it's not really clear why 
or how this happens.

print is like putstrln but implicitly coerces its argument using Show.
Actually, print is exactly equal to (putStrLn . show).


We say that putStrLn "returns unit".

2019-06-27
----------

## Exercises: Guard Duty

1.  If you put `otherwise` at the top, the other patterns will never be matched.
You always get an 'F' from this function.

2.  No, the first case matches instantly and short-circuits the rest of them.

3.  b, the function returns true when it's a palindrome.

4.  It can take any list whose members have an instance of Eq.

5.  pal :: (Eq a) => [a] -> Bool

6.  c, the function returns the sign of its argument.

7.  It can take things with Ord, but they also have to be usable with 0, which
is of Num type.

8.  Therefore the type signature is

numbers :: (Ord a, Num a) => a -> Integer



## 7.7 -- Guards

The syntax for guards looks weird.

The guard case expression must evaluate to bool.
otherwise is literally directly equal to True, used in guard expressions
as a catch-all.

It seems very much like the case expression, why is it here?  Actually, it seems
that case expressions are pattern matches, not arbitrary exprs.
Yes, this is true.  So guards are strictly more powerful than case exprs.

If your guard doesn't match, you are going to get an error about 'non
exhaustive' patterns.


1.  `dodgy 1 0` => 1
2.  `dodgy 1 1` => 11
3.  `dodgy 2 2` => 22
4.  `dodgy 1 2` => 21
5.  `dodgy 2 1` => 12
6.  `oneIsOne 1` => 11
7.  `oneIsOne 2` => 21
8.  `oneIsTwo 1` => 1+(2*10) => 21
9.  `oneIsTwo 2` => 2+(2*10) => 22
10. `oneIsOne 3` => 31
11. `oneIsTwo 3` => 23`

All CORRECT

dodgy is partially applied to the value one, meaning that the value will be 1+(1*10)
If dodgy is flipped, the second argument, the coefficient,  will be fixed to 2,
and the other one (the offset / intercept) will be the variable.

2019-06-26
----------

Note that we can derive Ord on a sum type.  This will enable us to compare
values, as happens in the example `compare True False`.

A really needlessly obtuse example in many ways, but it work well enough to
demonstrate the power of accepting comparison functions.


2019-06-20
----------

## Case expressions

Bool itself is a sum type.  It's the sum of two data constructors.

Case basically allows you to pattern match on a result type from an expression,
within a function at any time.  Eg,

    case EXPR of
      PAT1 -> RESULTEXPR

There is also a syntax case...where, the WHERE clause will bind names that will
become visible in the case expression.

## Case expression exercises

1.  See definition of `functionC` inside ExercisesChapter7.hs.

2.  See definition of `ifEvenAdd2`.  Also note that this needed to have the
    Integral typeclass applied in order to use `mod`.

3.  Just handle the case where Ordering is `EQ` and return some value.  
Note that we need both Num and Ord for this case.  All Num are not Ord.

## Higher order functions

Flip is going to apply its function with its two arguments flipped.  It must
have two arguments.  But note that flip applies the function all the way.

It's pretty easy to implement, the type signature is the hardest part.

A&M go on to say that type signatures can denote HOFs and that the explicit
associativity is right.  Hence currying is 'implicit'.


2019-06-19
----------

## Pattern matching tuples

You can pattern match tuples in the obvious way.
ghci contains a command :browse that you can use to list 'stuff' that's defined
in any module.

## Exercise: Variety Pack

1.

a)  The type of k is:

    k :: (a, b) -> a

CORRECT

b)  The type of k2 is `[Char]`

CORRECT

It is not the same type as k1 and k3.

c) k1 and k3 will return the number 3 as a result.

CORRECT

2.  My definition:

    f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
    f (x, x', x'') (y, y', y'') = ((x, y), (x'', y''))

 

2019-06-18
----------

`newtype` is a special case of data declarations.

Why?

`newtype` only allows one constructor with one field.  It's basically syntax
sugar for making a 'wrapper' type?   Whereas `data` can store multiple fields.

`UnregisteredUser | RegisteredUser` is a 'sum type'.  It
creates these specific data constructors, since they did not already exist.
The first argument of `data` is the specific data constructor to create.  They
are created as functions in the global namespace.  It's not really an enum
though, because the data constructors can have arguments.

This:

    data User = UnregisteredUser | RegisteredUser Username AccountNumber

Parses as this:

    data User = UnregisteredUser | (RegisteredUser Username AccountNumber)

It does NOT parse as the following, which would sort of make sense but is not
how Haskell operates:

    data User = (UnregisteredUser | RegisteredUser) Username AccountNumber

So this means that there are two data constructors created:

* UnregisteredUser, with no fields
* RegisteredUser with two fields

and the type User is the sum of these.

The odd thing is that there's no way just to see if an expression compiles, if
it doesn't have a show type.

It sucks to create a registerd user.

    printUser $ RegisteredUser (Username "foo") (AccountNumber 3337)

There would be an easier way to do this, just with a function.

Note that we can use data consructors within data constructor patterns, when
matching on product types:

    isGalapagosPenguin (Peng Galapagos) = True


2019-06-17
----------

## Anonymous functions

To syntactically disambiguate anonymous functions, you frequently need to put
them inside parentheses, eg `foo = (\x -> x * 3) 3`, now foo evaluates to 9.

## Exercises: Grab bag

1.  They are all identical.  Specifically they all expand to mth4.  CORRECT.
2.  d, because one arg was applied.  CORRECT.
3.  

a) See `addOneIfOdd2`.  CORRECT
b) See `addFive2`.  CORRECT
c) See `mflip2`.  CORRECT


## Pattern matching

Note that data constructors eg `Nada x` are referred to as constructors.  This
is the opposite of a deconstructor!

Patterns are matched against values and data constructors, but not types.

"Pattern matching proceeds from left to right, outside to inside" -- not really
clear what the practical implications of this statement would be.

The function isItTwo is a bit interesting because it matches a literal value.

The question really becomes, how is the match on 2 implemented here?
Is it terms of the Eq typeclass?  I don't think so because if I define a
data constructor that has multiple valid symbols, we can still compare them
even though we don't derive Eq.  There must be a more basic notion of identity
that is being used in this case.

Oh, the fault with this is simple.  The fact is that `Blah` and `Woot` become
DATA CONSTRUCTORS: they become first class and are just checked that they refer
to the same data constructor.  They are not values and are not checked according
to the rules of values.

There is a value called 'bottom' which is special in some sense, and will
abort the program.  This would seem to be similar to the 'exploding' value
that I've frequently wanted.

2019-06-14
----------

# Chapter 7: More functional patterns

Haskell functions are fully first class entities.
Saying

    f x = 42

Is exactly equivalent to giving the variable `f` a function value, much as in
Scheme and Clojure.

When applying functions, we can say that the actual argument is *unified with*
the formal parameter of the function.

If we have a bare value, eg `myVal = 24`, we know from the type that it has no
parameters as there isn't any `->` sign within the type.

When defining functions without a type hint, Haskell will infer the type of
arguments based on what's being used.  eg if you add something to an integer,
the argument is automatically inferred to also be an integer.

As is well known, multiple argument functions are just syntax sugar for single
argument functions.  This implementation leaks through the abstraction in the type
syntax, ie `a -> a -> a`, not `a a -> a`.  But the term level definition bundles
the arguments together, eg `f x y`.

If you add more arguments, ghc will infer a DIFFERENT parametrically-polymorphic
type variable for each one.  "The type variables are different because *nothing
in our code is preventing them from varying*" (emphasis mine).

When applying a function:

* Type variables (at the type-level) become bound to a type, and
* Function variables (at the term-level) become bound to a value.

Binding using let and where will infer types in the same way, presumably.
Let induces a new scope, much as in Scheme.

Shadowing with let works, much as it does in Scheme, and it does not generate
any error or warning.  That is, Haskell has lexical scoping.

Shadowing in ghci works similarly, this is what seems to be 'reassignment' in
heavy quotation marks.  In reality, every iteration of the READ/EVAL step in ghci
evaluates within a new let expression.

Anonymous "lambda" functions are defined with a backslash.  The stuff after the
backslash constitutes an entire argument list (with destructuring?  not sure
about this, we haven't done much of that yet)


2019-06-07
----------

You must write an instance of inherited superclasses before you are allowed to
write the child type class instance.  Eg this will be acceptable:

    newtype Nada = Nada Double deriving (Eq, Show)

    instance Num Nada where
      (+) (Nada x) (Nada y) = Nada (x + y)

    instance Fractional Nada where
      (/) (Nada x) (Nada y) = Nada (x / y)


Although it will generate warnings.  

Without the Num implementation, the compiler will whine and refuse to load the
code.

    • No instance for (Num Nada)
        arising from the superclasses of an instance declaration


Note that the entire type class did not need to be implemented, this one-method
implementation was enough to satisfy ghc.

## Type kwon do

1.  chk is a function that takes another function -- a type converter.
this has the ability to go from one type to a type with an instance of eq.

It could be more verbosely named `transformAndCompare`.

2.  arith is a 3 argument function.  Its args are as follows.

A function converting from unconstrained type variable a to b
One thing that you can do is to just ignore all the arguments.
You can also just apply the first argument, which is a conversion function, to
the third argument, and return the result.
But what can you do with the second argument, the Integer?
That much is extremely unclear.
The answer is that you can use the `fromInteger` method of the `Num` typeclass
to take it back to a plain Num.
And indeed I have confirmed with an upstream that this is the intended approach.

## Match the types

1.  Trying to assign to a binding that's hinted as :: a does not work.
Parametrically polymorphic variable
For the value `1`, ghci infers `Num a => a`.

2.  ghci infers `Fractional a => a`.  I think this will succeed.
No, it does not.  It's really not obvious to me why this fails.  Update, the
answer is that you can't assign a narrower type to the binding.  That would
mean that multiple concrete types would be being used.  The value has to
already be Num a => a.

3.  This succeeds.

4.  This succeeds because the Float has an instance of Real.

5.  We define identity function under the name `freud`.  Change it to Ord.
This should succeed.  And it does.

6.  Freud-prime.  Rebrand to Int -> Int, still works, bexause it's more
    specific.

7.  Try to modify a Int->Int function to be parametrically polymorphic again.
Will it work?  No, because user could pass any value in to the argument,
which would bind the type variable a to something else but would return Int.
And indeed that's correct.

8.  Should we be able to modify sigmund' to loosen restriction to Num a => a?
No, because myX is already a more specific type.   CORRECT.

9.  jung function returns the minimum element of a list.  And redoing it to Int
does work, because Int has an instance of Ord already.

10.  We design a version that's specific to strings.  Then attempt to loosen the
     restriction.  Of course this will work.  It's the exact same signature
as jung.  CORRECT

11.  mySort won't match as the type passed in is Ord a => [a].  But mySort
     require the more specific [Char] type.


2019-06-03
----------

## 6.13 Gimme more operations

The point is the compiler tells you when you lack the appropriate type class.
If I'm missing the type class constraint for an operation on a function, it's
going to say "Could not deduce (Ord a)" bound by the type signature.
And it's even going to tell you the possible fix.

Concrete types imply all the classes they imply.  This is pretty much self
explanatory.

You should always use the loosest type you can.  Only use type class constraints
that provide the operations which you actually use in the function.

## Exercises

### Multiple choice

1.  The Eq class

c) makes equality tests possible.

2.  The type class Ord

b) is a subclass of Eq.

3.  If a type class Ord has an operator >, what is its type.

a) Ord a => a -> a -> Bool

None of the others are realistic at all.

4.  In x = divMod 16 12

c) the type of x is a tuple, specifically Integral a => (a, a)


5.  The type class Integral includes 

a) Int and Integer numbers

It also includes 'Word' by the way.

## Does it typecheck?

1.  I don't believe that this will type check, because the person type has not
been declared as deriving show.  -- correct.
The fixed version destructures the Person type in the term and just shows the
Bool value that has been wrapped instead.

    printPerson :: Person -> IO ()
    printPerson (Person someVal) = putStrLn (show someVal)


2.  I believe that this will work.  I don't believe that it even has anything
to do with Show.
Wrong!  It doesn't work because we don't derive Eq, thus preventing us from
using Eq.
So Mood is correctly inferred.

Fixed version:

    data Mood = Blah | Woot deriving (Show, Eq)

3. 

a) Only Mood is an acceptable input.

b) The compiler will complain that "No instance for (Num Mood)", although I
honestly can't translate this message very well.

c) No instance for (Ord Mood).  That makes sense, as > has to type match on its
arguments.

4.  The only question really is whether s1 will type check.  I don't think it
    will, because it's missing the third data item, Object.

Fuck, it does!  This is because of currying.  The `Sentence` constructor
remains a function, not a bit of syntax.  Therefore, s1 is actually a function
Object -> Sentence.  It's a partially applied function and (as such) can't be shown.
However, the fully created Sentence works fine.
It's clear that the keyword `type` is being used to define sentences.  


## Given a datatype declaration, what can we do?

1.  I think that this will not typecheck, because the two arguments of the Papu
data constructor are not being explicitly coerced to the correct type.  Unless
there is some type of compiler magic that forces it to DWIM. -- CORRECT!

2.  As before, this is an example of the general case of not enough arguments
to the data constructor.  The first arugment type checks so the expression
will type check, but it yields a function `Yeah -> Papu`.  -- CORRECT!

3.  This should type check because the Papu type as a whole derives the Eq.
And it does -- CORRECT!

4.  This won't type check because Papu doesn't implement Ord.  -- CORRECT!

2019-05-31
----------

## Enum

This has methods succ and pred.
As well as a bunch of other confusing type conversion shit.

    > succ 6
    7

The `succ` of 4.5 is 5.5, which is not that clear to me.

enumFromTo gives you an inclusive range from one to the other.

    enumFromThenTo 1 10 100
    [1,10,19,28,37,46,55,64,73,82,91,100]

This is kind of weird.  What does it do?  It's adding 9 each time.  So I think
what it does is take the second argument as the first point in a series.
Then extends that series to the end point.  Although it doesn't always actually
reach the end point.  It stops short, but never goes over.

This is actually pretty useful, it's just constructing a regularly ordered
sequence based on an example.

## Show

This is used to create showable values.  Honestly the presence of this type
class is pretty frustrating but now I understand why.

Show is not a serialization format.  It's only for human readable data, not
for parsability.  It's a write only format, any readability is purely
coincidental.

The type class methods that Show has are.

showsPrec which I'm guessing shows to a certain precision.

show :: a -> String which is pretty obvious.

You can write a Show instance like so.

    instance Show MyType where
      show x = "Some representation"

How do we read some shit like this?

    instance Show a => Show (Maybe a)

Here we need our type variable that's attached to our Maybe<A> to be qualified
correctly.  That is, it needs to be Showable.

## Printing and side effects

`it` represents the last result evaluated in ghci.

The type of print reveals that it is effectful.  What's the type of `print`?

    print :: Show a => a -> IO () 	-- Defined in ‘System.IO’

The type written `IO ()` is an "IO action" that returns a value of the type `()`.
`()` is called "unit".  it's essentially a null.
So the part after IO indicates its return value.

So a value of type IO X indicates an action that will eventually produce an X.
It's not an X right now, but a delayed way to get an X.

The point so far is just that ghci implicitly calls an IO action when we evaluate
values.  This is `print it.`

## Read

Read is the opposite of Show but it's bad.

read can return "no parse".  It throws exceptions.  That makes it a "partial
function".  A function that's not guaranteed to yield a result that matches
its type.

## Instances are dispatched by type.

This means that if you have two types that define instances of the same class,
the type class methods aren't prefixed.  So ghci has to know which one to call.

If you have some type class method with no argument, then it just can't return
anything, because no type classes were matched.  You can hint to get the correct
type.




2019-05-23
----------

    compare True False => GT

Weird!

max and min are binary functions that work as you would expect.

It's not that clear what a tuple ordering does?

2, 0 is greater than 0, 3

so that means that the first result can take priority

the function is defined for lists which is pretty crazy

    instance Ord a => Ord [a] -- Defined in ‘GHC.Classes’

Hey, we finally talk about the `Show` error at a random time!  Luckily I've kind
of figured this out.

You can derive Ord which basically gives you a 'default' implementation based
on something weird.

it's possible to derive Ord.

But you can only derive Ord if you have eq.

The type system doesn't prevent you from making completely baffling behaviours,
though. --  'you want your ord instances to define a sensible total order'
this is presumably the type of thing that could be done with Idris

Because Eq is a superclass of Ord, using an Ord constraint gives you access to
the methods defined in the Eq class as well.  So Ord implies Eq.

    check' :: Ord a => a -> a -> Bool
    check' a a' = a == a'


## Exercises: Will they work?

    max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])

It will type check and the second list is greater than the first, so the second
list will be returned.  The type of argument is `Num a => [a]`.

Actually it's the lengths, not the lists, so the greater length will be returned,
which is 5, and the result type is `Int`.

    compare (3 * 4) (3 * 5)

Type variable a is bound to `Num a => a`.  and the result will be `LT` because
the first is less than the second.

    compare "Julie" True

This should not compile.

    (5 + 3) > (3 + 6)

This reduces to `(>) 8 9` and the result should be `False`.






2019-05-22
----------

some notes on polymorphism with type classes and defaults

declare a variable that gets specialized into a specific concrete type


The expression `(x+)` designates an operator section over x.  Because x is
an integer, the type variable `a` in the type signature for `(+)` gets specialized
to the `Integer` type.

"Monomorphism" means restrictedness as in 'type concreteness'.

    let add = (+) :: Integer -> Integer -> Integer

I don't really understand what the `let` keyword is being used for in this
context.  Is it a ghci expression?  How is `let x = y` different from `x = y`?

This expression basically shows that you can 'downcast' from the more polymorphic
type to the less polymorphic type.  But you can't go the other way (why not?)
This is a difference between the way matching works at typelevel and the way
that it works at term level.

## Ord

ord defines no really interesting methods.  Note max and min are two-argument
functions unlike the schemey versions.  `compare` is an interesting function
that has the signature

    compare :: a -> a -> Ordering

What is an Ordering?

The answer is:

    data Ordering = LT | EQ | GT

This could be fun to play with.

We know that for instance Char is orderable.

Let's try it

`compare a z` yields `LT` which is the showable constant

    bar :: Ordering -> String
    bar LT = "Less than"
    bar GT = "Greater than"
    bar EQ = "Equal"

Test it with

    bar (compare 'a' 'z')

This worked well.

Now let's return to the type class definition for Ord.


    class Eq a => Ord a where
       ...

Here what do we note -- we note that the Ord class is constrained to those
type that have an instance of Eq already -- thus enabling the use of Eq's
methods.  not that they would always be usable, but.

My question is, is it necessary formally for Ord to be constrained by Eq?
Or is that a convenience? -- see logs folder

2019-05-21
----------

signum is a method on a Num a => a that will give you its sign, -1, 0, or 0.

Why is fromInteger present on the type class?
It s pretty unclear

The existence of a type `fromInteger Integer => a` implies that the 'return
value', insofar as it exists, is also a site of type inference.

We can verify this 

    foo :: String -> Float
    foo _ = fromInteger 42


Here, Float's instance of the `Num` method `fromInteger` is being called.

We haven't actually looked at delcaring our own classes yet.  So it's not
totally clear what the meaning of requesting type class instances on the *class*
declaration itself is.

## Exercise: Tuple Experiment

Look for quotRem and divMod.

    quotRem :: a -> a -> (a, a)

It obviously returns the quotient and the remainder as a tuple.

    divMod :: a -> a -> (a, a)

Does a similar thing except that `quot` truncates towards zero, while `div`
truncates towards negative infinity.

    ones x = snd (divMod x 10)

A function that takes one argument and divides by 10 returning the remainder.

eg it's effectively a limited-wrap-around-counter that wraps at 10.


## Fractional

Fractional has type class methods (/) for division, `recip` which is presumably
the reciprocal, and `fromRational`.

Not all num are divisible, only Fractional are.

## Put on your thinking cap

It is only necessary to specify that the argument should have an instance of
Fractional in order to implement `divideThenAdd` because all Fractionals are
Nums.

How do we know this?  We know it because of the type class declaration of
Fractional.

    class Num a => Fractional a ...

Because Fractional requires Num, you transitively get access to all the methods
defined on Num.

When using ghci the defaults specified for type classes will cause literals to
coerce to that specific type.



2019-05-20

Exercises: Eq Instances
-----------------------

1. 

    instance Eq TisAnInteger where
      (==) (TisAn x) (TisAn y) = x == y

2.

    instance Eq TwoIntegers where
      (==) (Two x y) (Two x' y') = (x == x') && (y == y')

3.

    instance Eq StringOrInt where
      (==) (TisAnInt x) (TisAnInt y) = x == y
      (==) (TisAString x) (TisAString y) = x == y
      (==) _ _ = False

4.   Pair of one parametrically polymorphic type:

    instance Eq a => Eq (Pair a) where
      (==) (Pair x y) (Pair x' y') = (x == x') && (y == y')

5.  Pair of two different parametrically polymorphic  needs two type class instance
on both type variables.

    instance (Eq a, Eq b) => Eq (Tuple a b) where
      (==) (Tuple x y) (Tuple x' y') = (x == x') && (y == y')


6.  Sum type of two parametrically polymorphic type variables.

    instance (Eq a) => Eq (Which a) where
      (==) (ThisOne x) (ThisOne y) = x == y
      (==) (ThatOne x) (ThatOne y) = x == y
      (==) _ _ = False

We only needed to specify the type class constraint for the single type variable
even though it's used across two different data constructors.

7.  Sum type across two different parametrically polymorphic type variables.  We
need to specify type class constraints on both of them.  Note however that it's
only the actual content of the terms that we define -- `x == y` -- that leads
to the necessity to add these constraints.

    instance (Eq a, Eq b) => Eq (EitherOr a b) where
      (==) (Hello x) (Hello y) = x == y
      (==) (Goodbye x) (Goodbye y) = x == y
      (==) _ _ = False



2019-05-16

To make types printable, just add `deriving Show` to them.

A _partial function_ what is it?  Some function where all set of inputs are
not exhaustively covered by terms.

The language will throw a run time error if you miss out some clause.
`Non-exhaustive pattern in function`.

We can use `-Wall` to get more warnings which will warn us about this case.

You can set these parameters in ghci using `:set` command.
It's not clear how to set them on a permanent basis.

You have .ghci file local to a project and also ~/.ghc/ghci.conf.

ghc is clever enough to tell you which patterns are matched for some type.

    Pattern match(es) are non-exhaustive
    In an equation for ‘f’:
        Patterns not matched: p where p is not one of {2}

It knows that 2 is the only value of the type `Int` that is matched.
You use the wildcard underscore `_` to match all cases.

The key is to not use Int to represent enums, otherwise you're going to have
to match all cases everywhere you want to only care about a restricted set.
Just declare the set of values that you DO care about.

    data Identity a = Identity a

This will declare a wrapper type `Identity` for a parametrically polymorphic
type variable `a`.
How would we compare these?  We couldn't really because we haven't proved that
`a` has an instance of `Eq`.

I'd expect something like this to work.

    instance Eq Identity where
      (==) (Identity x) (Identity y) = x == y


But it doesn't, I get `Expected one more argument to Identity`.  Ah the syntax
is wrong.

    instance Eq (Identity a) where
      (==) (Identity x) (Identity y) = x == y

Note that we have to write `instance Eq (Identity a)` instead of
`instance Eq Date` as we were able to do so before.  QUESTION: why?

The correct way is as follows:

    instance Eq a => Eq (Identity a) where

How  is this to be read?  Compare it with:

    instance Ord a => Eq (Identity a) where


One thing you should notice here is that in the second example, it's still the
 `Eq` being declared.
So it should be read as

instance <TYPE-CLASS-CONSTRAINT> => <TYPE-CLASS> <CLIENT-CLASS>

where CLIENT-CLASS can also be parameterized over its type variables.
Note that we are already into what Java calls 'generics' here.
`Identity a` is a generic type equivalent to `Identity<A>` in java.

Perhaps the reason we have to write `Identity a` is related to this, that once
you have a polymorphic type in the name of an aggregate type, it's always
necessary to use the name of it, much in the same sense that it doesn't make
sense to say `List` in Java, you have to say `List<Foo>` or (at the least)
`List<?>`.

In another sense, obviously it wouldn't make sense to express a type class
constraint on a type that didn't have it.  And in fact this is an error.

    • Variable ‘a’ occurs more often
        in the constraint ‘Show a’ than in the instance head
      (Use UndecidableInstances to permit this)
    • In the instance declaration for ‘Eq Date’

So this tells us that the constraint  part is the part before =>.
The "instance head" is the part comprising  TYPE-CLASS CLIENT-CLASS

Chapter and verse:

> In Haskell 98 the head of an instance declaration must be of the form C (T a1
> ... an), where C is the class, T is a data type constructor, and the a1 ... an
> are distinct type variables.

So if there are no type variables, it just collapses to `C (T)` that is
equivalent to `C T` I presume.  This is the case in the `Eq Date` example, C =
`Eq`, T = `Date`.

2019-05-15

To access fields on our 'record' type, we just destructure it in the definition
for the instance.

    instance Eq Date where
      (==) (Date weekDay1 dayOfMonth1) (Date weekDay2 dayOfMonth2) =
        weekDay1 == weekDay2 && dayOfMonth1 == dayOfMonth2



2019-05-14

## Ch6: Type classes

"Type classes allow us to generalize over a set of types in order to define and
execute a standard set of features for those types."

There exists a type class named Bounded.  It's not clear what functions this
type class provides.  (You can look it up in zeal, it's part of the Prelude.)

Type classes that are in the prelude exist in some hierarchical relationship,
because you need to be able to Eq something to be able to Ord it.

Not every type can be compared for equality.  One example is functions.  No
function should ever be equal to any other function.


Let's try to compare two functions.  We get a message `No instance for (Eq ...)`.

You can look up information on type classes using the `:info` operator.  The basics
though are just the following:

    class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool

So if a type wants to have an instance of Eq, it just defines these functions
for itself.  :info is also going to tell us which types have an instance.

Equality is defined for tuples of various lengths up to 14.

Question: How does == work on lists?
It's not clear that [Char] has an instance of Eq.
Actually it does -- it's listed here:

    instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’

Not quite sure how to read this yet.  Is this just a type signature?

You can't compare different types.  This won't work:

    (1, 2) == "puppies"

Because the concrete type assigned to the type variable `a` is set by the leftmost
occurrence, so in this case, the type of `a` is `(Integer, Integer)` and there's
now no matching function that will work with [Char].

    instance (Eq a, Eq b) => Eq (a, b)

> Critically, the Eq instance of (a, b) relies on the Eq instances of a and b.
> This tells us the equality of two tuples (a, b) depends on the equality of
> their constituent values a and b.

Is this actually the implementation, or is it just the specification?  It's
unclear.

    data Trivial = Trivial

A new type starts out with zero type class instances.  It can't be printed and
really you can't do anything useful with it except for pass it to the identity
function.

Data constructors and type constructors can have the same name which can get
confusing.  In this example the type constructor is `Trivial` while the data
constructor is `Trivial'`.  Data refers not to the keyword that is used to
introduce it, but the string that's used at *term-level* to create the data.





















































2019-04-16

## Ch5: Definitions

_Polymorphism_ simply describes a type variable that can take on more than
one concrete type.  That is, in a type signature, anything that's not a concrete
type is polymorphic to some degree.

_Type inference_ is where terms do not have an explicit typing but the compiler
is able to deduce them.  This isn't always the case, but it is often the case.

The _principal type_ is the most polymorphic type that still typechecks.  When a
system uses _principal typing_, the type system produces a set of types as a
result of inference.  The principal type is the least constrained one.  For
instance, if a parametrically polymorphic type is present (plain `a`) then that
type will be the principal type.  If that's not available, then a constrained
type will be chosen, if available, etc.

A _type variable_ is a way of referring to a type or a set of types throughout
a type signature.  In general, their scope is the entire type signature.  So
in the signature `a -> b -> a` the second `a` refers to the same set of types
as the first `a` does.

A _type class_ is a way to describe operations that multiple types can
implement.

_Parametricity_ is unconstrained polymorphism.  It means that the function will
have the exact same behaviour for every input type.

_Ad-hoc_ polymorphism is another word for polymorphism _constrained_ by a type
class.  Parametric polymorphism can tell us a lot about the behaviour of a
function.  For instance:

    f :: a -> [a]

The ONLY thing this function can do is return a list of its value, repeated a
constant number of times.  Why?  Because using the parametrically polymorphic
value `a` we can't transform the value in any way.  So it has to be the same
value.  Because there's no input specifying the list, the length of the list
MUST be hardcoded.  It can't be deduced from the input because there are no
operations for `a`, and because functions are pure there's no way to get
any other information that is not itself constant.  It's not like we could,
for instance, return a list repeated a random number of times, because this
would cause us to enter the IO or State monad (I believe).

A _module_ organizes and scopes:

* Values
* Functions
* Data types
* Type classes
* Type class instances

The keyword `import` has to live at the top of the file, before the definitions
start.

The distinction drawn here between _parametric_ and _ad-hoc_ polymorphism is
derived from Strachey's _Fundamental Concepts in Programming Languages_ text.

2019-04-15

### Does it compile?

1.

It does not compile.  The error happens in 'wahoo'.  Bignum is just a big
number.  It's not like an operator section.  I don't know what this code
is trying to do, so I don't know how to fix it.

Actually there is a totally different error when I try to do, a bunch of junk
about both lines, so not sure on this.

This actually does compile in ghci.  The use of NoMonomorphismRestriction changes
the behaviour and the error message.

I don't totally understand the point of this problem, but one fix is this:
Assume that `bigNum` was actually supposed to be a function:

    bigNum = (^) 5
    wahoo = bigNum $ 10


2.  The relevant code is:

    x = print
    y = print "woohoo!"
    z = x "hello world"

Yes, it does compile, so I suppose that nothing needs to be fixed.

3.  This shouldn't compile, because the term for `c` is incorrect.

    a = (+)
    b = 5
    c = b 10
    d = c 200

I suppose that it's trying to use the aliased function at some point.

4.  This doesn't compile, because c0 isn't defined.

### Type variable or specific type constructor?

2.

`zed` -- Fully polymorphic type variable
`Zed` -- Concrete
`Blah` -- Concrete

3. 

`a` -- Fully polymorphic
`b` -- Constrained polymorphic
`C` -- Concrete

4.

`f` -- Fully polymorphic
`g` -- Fully polymorphic
`C` -- Concrete

### Write a type signature

1.  This function takes a list of any type and returns one element of that type.

Therefore its type should be `[a] -> a`.

2.  This function takes two arguments of a type that can have the > function
applied to them.

The type of the > function is:

    (>) :: Ord a => a -> a -> Bool

Therefore the type of `functionC` should be:


    functionC :: (Ord a) => a -> a -> Bool

3.  This function takes a 2-tuple as its argument, and returns the second element
of the tuple.

Therefore its type should be:

    functionS :: (a, b) -> b

### Given a type, write the function

1.  The only definition is the identity function.

    i :: a -> a
    i x = x

2.  The sensible definition of this one is:

    c :: a -> b -> a
    c x _ = x

3.  Yes, in this example `c''` is the same thing as `c`.

4.  This is just a very similar version to the other one.

    c' :: a -> b -> b
    c _ y = y

5.  This is just a list transformer.

One successful solution is `r x = x`.
Another solution might be `r = x ++ x`.

6.  Here our eventual aim is to get the `c` out.
We only have one possible definition.

    co :: (b -> c) -> (a -> b) -> a -> c
    co f1 f2 x = f1 (f2 x)

7.  This is just a troll example.  You just ignore the first argument.

    a :: (a -> c) -> a -> a
    a _ x = x

8.  You just use the conversion function to convert the value argument.

    a' :: (a -> b) -> a -> b
    a' f1 x = f1 x  


### Fixit

1.  Done in Sing.hs.  `fstString` should actually be a function from string
to string, you can't use operators in types, as ghci helpfully informs you.
It foolishly used 'or' instead of the real syntax for alternatives, `else`.
It also used the same variable names for `x` and `y`.

2.  Just change `>` to `<` or vice versa

3.  Done in Arith3Broken.hs.  Problems are:

* `main` was capitalized when it should not have been.
* `print 1 + 2` won't work because of precedence
* putStrLn won't work because it needs an Integer
* The literal `-1` needs to be parenthesized

### Type-kwon-do

1.  h :: Int -> Char

You can compose g and f, like `h = g . f`

2.  The same approach here.

3.  You destructure in the pattern match and apply the functions to convert
the types.

    xform :: (X, Y) -> (Z, Z)
    xform (x, y) = (xz x, yz y)

4.  The final output variable is `w`.  So we need to do this:

munge f1 f2 x = 
x at term level is bound to the type variable x
f1 turns x to y
f2 turns it to (w, z)
Then you just use `fst` to extract the `w` one.



2019-04-12

## 5.8 Exercises for chapter 5.

### Multiple choice

1.  A value of type `[a]` is:

c) A list whose elements are all of some type `a`.

2.  A function of type `[[a]] -> [a]` could:

a) take a list of strings as an argument

3.  A function of type `[a] -> Int -> a`:

b) returns one element of type `a` from a list.

4.  A function of type `(a, b) -> a`:

c) takes a tuple argument and returns the first value.

### Determine the type


1.

a) (* 9) 6

I can only assume that this does an operator section.  And that it will return
9*6, ie 54.  But it should -- SHOULD -- return a `Num a => a`.

CORRECT

    dtt1b = head [(0, "doge"), (1, "kitteh")]

I think that this should return a tuple type inferred based on the first pair.
So the result should be of type `(Num a => a, [Char])`

The result was right, but it's actually written in a more logical way,

    Num t => (t, [Char])

Remember that the `=>` acts as a delimiter and typeclass constraints are
always listed first!

c) 

    dtt1c = head [(0 :: Integer, "doge"), (1, "kitteh")]

This is quite a confusing one.  Bear in mind that a list always has to be
of a homogenous type.  So because the second item in the list was not hinted
to be integer, the result should be

    Num a => (a, [Char])

WRONG -- Somehow the compiler is smart enough to realize that the result is

    dtt1c :: (Integer, Char)

How?  The key lies in the fact that the type of this expression:

    [(0 :: Integer, "doge"), (1, "kitteh")]

Is actually:

    [(Integer, [Char])]

The first element having been coerced to Integer, AND the second being coercible
TO integer, ghc just makes the whole list integer.

d)

    dtt1d = if False then True else False

Assuming a smart compiler, you could just say that the result is `False`.  But
`False` is not an expression at type level.  So that means that the result
should simply be of type `Bool`.

CORRECT

e) 

    dtt1e = length [1,2,3,4,5]

The type of this should be Int.  Just because I happen to know that it's 5, I
also know that the result of `length` is always `Int`.  And indeed it is.

CORRECT


2.  What is the type of w?

Almost certainly `Num a => a`.  CORRECT

3.  The type of z should be

Num a => a -> a

CORRECT

4  What is the type of f?   It can be `Fractional a => a`.

CORRECT


5.  What is the type of f?  It should be `[Char]`.  

CORRECT


## 5.7 Asserting types for declarations

The :: type hinting syntax at term level can be parenthesized.  Eg these are
valid and loadable by ghci.

    inferredButRestrictedTriple x = (x * 3) :: Integer

And also this

    inferredButRestrictedTriple x = x * (3 :: Integer)

These two get inferred to being the same eventual type signature.
You can also use local type declarations as part of a 'where' expression.

    tripleWithLocalTypeDeclaration x = tripleItYo x
      where tripleItYo :: Integer -> Integer
            tripleItYo y = y * 3

The type of `tripleItYo` will automatically be propagated to the function
that's calling it.  So it gets inferred correctly to the using function.

You can't just wantonly hint that, for instance, a numeric literal is actually
a string.  `5 :: String` will just blanket fail.

There's a good use of 'overdetermined' here, for when one trivial error may be
masking another error.

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


=======
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



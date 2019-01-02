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

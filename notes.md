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

Reloading will change definitions.























































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

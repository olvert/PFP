#### DAT280 – Lab D: “Laziness considered harmful for parallel programming”

*Olle Svensson (ollesv@student.chalmers.se)*

*Agazi Berihu (agazi@student.chalmers.se)*

## Laziness considered harmful for parallel programming

### 1. What is Laziness?

In functional programming, we can define lazy evaluation as, an evaluation strategy where arguments are evaluated if and only if another computation needs them. To understand how lazy evaluation works let us consider the following code in Haskell:

```
prelude> let x = 14 * 21
```

In order to follow this tutorial, we need to override Monomorphism using this following command line in ghci:

```
prelude> :set -XMonomorphismRestriction
```

To see the value of x we can use sprint: The sprint command prints the value of the variable without doing any computation:

```
prelude>:sprint x
x = _
```

We can see that x is not evaluated yet. To evaluate the value of x fully, we can just make simple print statements. Since the result of the evaluation is needed for print, the multiplication operation has to be evaluated and assign the value to x.

```
prelude> print x
294
```

To see if the value of x is evaluated, we can try sprint. 

```
prelude>:sprint x
x = 294
```

Laziness has its own advantages and disadvantages. One of the main advantages is avoiding unnecessary evaluation of arguments hence increasing efficiency. But this is not always true.

### 2. How to override laziness to use parallelisation?
In order to parallelise a functional languages like Haskell, one need to override the underlying lazy operations. Examples used are this tutorial are taken from [Parallel and Concurrent Programming in Haskell by Simon Marlow](http://chimera.labs.oreilly.com/books/1230000000929/ch02.html)

For instance in Haskell this is expressed using the Eval monad, which is includeded in the module Control.Parallel.Strategies. The Eval monad is defined as follows:

```
data Eval a
instance Monad Eval

runEval :: Eval a -> a

rpar :: a -> Eval a
rseq :: a -> Eval a
```

As we can see from the definiton above Eval monad has two operations, rpar and rseq. The rpar function says, "My argument could be evaluated in parallel". Thus rpar allows argumetns to be evaluated in parallel. But the rseq function says opts for sequential evaluation. The rseq function can says, "Evaluate my argument and wait for the result."

Let us take the fibonacie function to show how rpar and rseq work. The fib function defiend below returns the fibonacie of a given number. 

```
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

We can have different combination rpar and rseq. 

##### First variant is to use only rpar.
```
runEval $ do
    a <- rpar (fib 14)
    b <- rpar (fib 21)
    return (a,b)
```
In the above implementation, fib 14 and fib 21 are evaluated in parallel. Since we are using the rpar, the return does not wait for the evaluation of fib 14 and fib 21 to complete, and it should run.

To try those examples (we assume the file is saved as tutorial.hs), first it has to be compiled in parallel by adding -threaded flag. This flag tells the compiler to use GHC parallelism. Then we run our biinary with two cores, which is enforece by the flags +RTS -N2.

```
$ ghc -O2 tutorial.hs -threaded
$ ./tutorial +RTS -N2
```

##### Second variant is to use both rpar and rseq.
```
runEval $ do
    a <- rpar (fib 14)
    b <- rseq (fib 21)
    return (a,b)
```
This variant of combination rpar and rseq solves the both fibonacies in parallel, but since we used the rseq to get the second evaluation, the return waits for evaluation of the second fibonacie to finish. After fib 21 is gave result, it will return.

##### Third variant is to use only rpar.
```
runEval $ do
    a <- rpar (fib 14)
    b <- rseq (fib 21)
    rseq a
    return (a,b)
```
This is similar to the previous vairiant, but we have added rseq a, which tells the return to wait for evaluation of fib 14. 

The following is similar with the previous one, where the return waits for both evaluations before returning.

```
runEval $ do
    a <- rpar (fib 14)
    b <- rpar (fib 21)
    rseq a
    rseq b
    return (a,b)
```

Depending on the problem we are trying to solve, we can use the different combination of rpar and rseq. For instance, in the previous examples we have to decide whether to go sequentially or parallelly. Thus it could hurt the performance of the parallel program. Even though laziness adds modularity for parallel programming, it is not straight forward and easy to use.

#### Resources used to prepare this tutorial

[Parallel and Concurrent Programming in Haskell by Simon Marlow](http://chimera.labs.oreilly.com/books/1230000000929/ch02.html)  
[To be or not to be. . . lazy (In a Parallel Context) by Mercedes Hidalgo-Herrero](http://ac.els-cdn.com/S157106610900485X/1-s2.0-S157106610900485X-main.pdf?_tid=a5704802-3a7c-11e7-aaa6-00000aacb360&acdnat=1494969448_c7afca7e1a6ff7c6cc3aa5e8ae9f952e)  
[The Impact of Laziness on Parallelism and the Limits of Strictness Analysis by G. Tremblay and G. R. Gao](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=768DA70A368C0C513BBED1DDCF553B51?doi=10.1.1.57.3751&rep=rep1&type=pdf)
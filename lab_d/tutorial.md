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

### 2. The Eval monad and Deepseq
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

To try those examples (we assume the file is saved as tutorial.hs), first it has to be compiled in parallel by adding -threaded flag. This flag tells the compiler to use GHC parallelism. Then we run our binary with two cores, which is enforced by the flags +RTS -N2.

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

This variant of combination rpar and rseq solves the both Fibonacci's in parallel, but since we used the rseq to get the second evaluation, the return waits for evaluation of the second Fibonacci to finish. After fib 21 gave the result, it will return.

##### Third variant uses both rpar and rseq.
```
runEval $ do
    a <- rpar (fib 14)
    b <- rseq (fib 21)
    rseq a
    return (a,b)
```
This is similar to the previous variant, but we have added rseq a, which tells the return to wait for the evaluation of fib 14. 

The following is similar with the previous one, where the return waits for both evaluations before returning.

```
runEval $ do
    a <- rpar (fib 14)
    b <- rpar (fib 21)
    rseq a
    rseq b
    return (a,b)
```
Depending on the problem we are trying to solve, we can use the different combination of rpar and rseq. For instance, in the previous examples, we have to decide whether to go sequentially or parallelly. Thus it could hurt the performance of the parallel program.

#### Deepseq

```
class NFData a where
  rnf :: a -> ()
  rnf a = a `seq` ()
```
The rnf function which is defined in Control.DeepSeq module stands for "reduce to normal-form." It fully evaluates a given argument and as it can be seen form the definition above returns (). In the next section, we will experiment with rnf.

### 3. Forcing lazzy evaluation
To observe how lazy evaluation can hurt parallelisation let us consider the quick sort implementation in Haskell. 

```
--tutorial2.hs

import Criterion.Main
import System.Random
import Control.Parallel
import Control.DeepSeq

-- Original quick sort
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y<x] ++ [x] ++ qsort [y | y <- xs, y>=x]

-- Paralle quick sort with par
psort [] = []
psort (x:xs) = par rest $ psort [y | y <- xs, y<x] ++ [x] ++ rest
    where rest = psort [y | y <- xs, y>=x]

-- Paralle quick sort by forcing the lazy evaluation using rnf
psort_ [] = []
psort_ (x:xs) = par (rnf rest) $ psort_ [y | y <- xs, y<x] ++ [x] ++ rest
    where rest = psort_ [y | y <- xs, y>=x]

main = defaultMain  
    [bench "qsort" (nf qsort randomInts),
    bench "head" (nf (head.qsort) randomInts),
    bench "psort" (nf psort randomInts),
    bench "psort_" (nf psort_ randomInts)]

randomInts =
    take 200000 (randoms (mkStdGen 211570155))
    :: [Integer]
```

In the above code snippet (which is inspired from Chalmers Parallel Functional Programming Slides), the qsort function is the general quick sort which doesn't incorporate any parallelisation. The psort function parallelizes the quick sort by using par operation. Finally the psort_ which uses rnf operation. To run the previous code:


```
$ ghc -O2 tutorial2.hs -threaded
$ ./tutorial2 +RTS -N2
```

After benchmarking, we get the following results:

```
benchmarking qsort
time                 437.2 ms   (423.3 ms .. 451.3 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 434.3 ms   (429.9 ms .. 436.2 ms)
std dev              3.830 ms   (0.0 s .. 4.119 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking head
time                 10.85 ms   (10.54 ms .. 11.10 ms)
                     0.995 R²   (0.992 R² .. 0.998 R²)
mean                 10.82 ms   (10.61 ms .. 11.09 ms)
std dev              643.0 μs   (432.2 μs .. 992.2 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking psort
time                 431.7 ms   (425.6 ms .. 439.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 432.5 ms   (431.2 ms .. 433.5 ms)
std dev              1.408 ms   (0.0 s .. 1.614 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking psort_
time                 373.0 ms   (231.4 ms .. 481.6 ms)
                     0.984 R²   (0.942 R² .. 1.000 R²)
mean                 354.7 ms   (326.0 ms .. 370.5 ms)
std dev              25.23 ms   (67.99 as .. 27.30 ms)
variance introduced by outliers: 20% (moderately inflated)
```

From the result, we can observe that psort_ which forces the lazy evaluation got a better benchmark (373.0 ms) than the parallel one (431.7 ms). Thus, even though laziness adds modularity for parallel programming, it does not always help improve performance.


#### Resources used to prepare this tutorial

[Parallel and Concurrent Programming in Haskell by Simon Marlow](http://chimera.labs.oreilly.com/books/1230000000929/ch02.html)  
[To be or not to be. . . lazy (In a Parallel Context) by Mercedes Hidalgo-Herrero](http://ac.els-cdn.com/S157106610900485X/1-s2.0-S157106610900485X-main.pdf?_tid=a5704802-3a7c-11e7-aaa6-00000aacb360&acdnat=1494969448_c7afca7e1a6ff7c6cc3aa5e8ae9f952e)  
[The Impact of Laziness on Parallelism and the Limits of Strictness Analysis by G. Tremblay and G. R. Gao](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=768DA70A368C0C513BBED1DDCF553B51?doi=10.1.1.57.3751&rep=rep1&type=pdf)
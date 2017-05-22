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

### 2. Why lazzines is harmful for parallel programming?

Laziness restricts parallel programming because as mentioned earlier arguments are evaluated when needed. Thus lazy evaluation can add overhead in parallel programming due to the wait for the full evaluation of arguments.

### 3. How to override laziness to use parallelisation?

In order to parallelise the functional languages like Haskell, we need to overrides lazy evaluations. In Haskell one method is Eval monad, which is used for parallel programming. Eval monad has two operation rpar and rseq. rpar says, "My argument could be evaluated in parallel" and rseq says, "Evaluate my argument and wait for the result."

In additon we can use Starategies and Eval monad, which are also used parallelising.

### 4. Can we achieve fine grain parallelisation using lazy language?

Fine grain paralisation is not achievable according to [The Impact of Laziness on Parallelism and the Limits of Strictness Analysis by G. Tremblay and G. R. Gao](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=768DA70A368C0C513BBED1DDCF553B51?doi=10.1.1.57.3751&rep=rep1&type=pdf).

#### Resources used to prepare this tutorial

[Parallel and Concurrent Programming in Haskell by Simon Marlow](http://chimera.labs.oreilly.com/books/1230000000929/ch02.html)  
[To be or not to be. . . lazy (In a Parallel Context) by Mercedes Hidalgo-Herrero](http://ac.els-cdn.com/S157106610900485X/1-s2.0-S157106610900485X-main.pdf?_tid=a5704802-3a7c-11e7-aaa6-00000aacb360&acdnat=1494969448_c7afca7e1a6ff7c6cc3aa5e8ae9f952e)  
[The Impact of Laziness on Parallelism and the Limits of Strictness Analysis by G. Tremblay and G. R. Gao](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=768DA70A368C0C513BBED1DDCF553B51?doi=10.1.1.57.3751&rep=rep1&type=pdf)

# DAT280 – Lab B: “Sudoku in Erlang”

*Olle Svensson (ollesv@student.chalmers.se)*

*Agazi Berihu (agazi@student.chalmers.se)*

## Attempts

### Improved version

The benchmark was done on a Macbook Pro with an Intel i5 with two physical and
four logical cores.

**Benchmark original:**
#### 1
```
{61530626,
 [{wildcat,0.34176999999999996},
  {diabolical,49.41115},
  {vegard_hanssen,110.95629},
  {challenge,7.76298},
  {challenge1,400.04997},
  {extreme,10.074},
  {seventeen,36.70979}]}
```
#### 2
```
{62286634,
 [{wildcat,0.34414},
  {diabolical,49.86796},
  {vegard_hanssen,110.67564999999999},
  {challenge,7.540970000000001},
  {challenge1,406.64354},
  {extreme,10.375290000000001},
  {seventeen,37.41853}]}
```
#### 3
```
{62169325,
 [{wildcat,0.37411},
  {diabolical,49.237269999999995},
  {vegard_hanssen,109.22297},
  {challenge,7.503939999999999},
  {challenge1,407.97423},
  {extreme,9.97407},
  {seventeen,37.40638}]}
```
**Geometric Mean**  =  cube_root(61530626 * 62286634 * 62169325)

            =  61994636.21109078

**Benchmark parallelized:**
#### 1
```
{32964905,
 [{wildcat,0.36256},
  {diabolical,26.315810000000003},
  {vegard_hanssen,64.80456},
  {challenge,4.752680000000001},
  {challenge1,182.14541},
  {extreme,16.75684},
  {seventeen,34.51041}]}
```
#### 2
```
{32618242,
 [{wildcat,0.37456},
  {diabolical,26.14618},
  {vegard_hanssen,64.13234},
  {challenge,4.80504},
  {challenge1,179.15288},
  {extreme,17.29643},
  {seventeen,34.27467}]}
```
#### 3
```
{32276927,
 [{wildcat,0.35885},
  {diabolical,24.72998},
  {vegard_hanssen,64.02642},
  {challenge,4.795229999999999},
  {challenge1,180.31195000000002},
  {extreme,15.927610000000001},
  {seventeen,32.618919999999996}]}
```
**Geometric Mean**   =  cube_root(32964905 * 32618242 * 32276927)

           =  32618815.504131433

**Improvement in %** = ((61994636.21109078 - 32618815.504131433) * 100 )/ 61994636.21109078

           = 47.40 %
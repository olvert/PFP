## Setup – macOS

### 1. Setup environment
```
cabal sandbox init
cabal configure
cabal install --dependencies-only
```

Using sandbox since some dependencies requires old versions.

### 2. Install GTK
Follow instructions [here](https://wiki.haskell.org/Gtk2Hs/Mac).

### 3. Install ThreadScope
Follow instructions [here](https://wiki.haskell.org/ThreadScope).

## Runs

### map (-N2 -A100m) ---Olle
Total time:   11.60s
Mutator time: 11.48s
GC time:      0.3s
Productivity: 98.9% of mutator vs total

### map (-N2 -A100m -H1g) ---Olle
Total time:   12.39s
Mutator time: 12.27s
GC time:      0.12s
Productivity: 99.0% of mutator vs total

### mapDaC depth: 2 (-N4 -A100m -H1g) ---Olle
Total time:   14.89s
Mutator time: 8.31s
GC time:      6.59s
Productivity: 55.8% of mutator vs total

### mapDaC depth: 2 (-N4 -A100m -H2g) ---Olle
Total time:   9.92s
Mutator time: 8.35s
GC time:      1.58s
Productivity: 84.1% of mutator vs total

### parMapD depth: 2 (-N4 -A100m -H2g) ---Olle
Total time:   9.59s
Mutator time: 7.45s
GC time:      2.14s
Productivity: 77.7% of mutator vs total

### parMapD depth: 2 (-N4 -A100m -H2g) ---Olle
Total time:   8.80s
Mutator time: 7.29s
GC time:      1.51s
Productivity: 82.8% of mutator vs total

### mapPseq (-N2 -A100m) ---Olle
Total time:   23.36s
Mutator time: 12.52s
GC time:      10.84s
Productivity: 53.6% of mutator vs total

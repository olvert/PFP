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

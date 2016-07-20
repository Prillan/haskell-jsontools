# jsontools

[![Build Status](https://travis-ci.org/Prillan/haskell-jsontools.png)](https://travis-ci.org/Prillan/haskell-jsontools)

## Installation
If you have `stack` installed, just run

```
git clone https://github.com/Prillan/haskell-jsontools.git
cd haskell-jsontools

# Download and install GHC
stack setup

# Build and install
stack install
```

### jgrep
A tool similar to `grep` for searching json files.

```
Usage: jgrep EXPRESSION [-v]

Available options:
  -h,--help                Show this help text
  EXPRESSION               Expression
  -v                       Verbose
```

TODO: Add example

### jextract
Extract json data.

```
Usage: jextract EXPR [-f|--flat] [-p|--pretty-print] [-v|--values-only] [FILE]

Available options:
  -h,--help                Show this help text
  EXPR                     Projection glob
  -f,--flat                Flatten result
  -p,--pretty-print        Pretty print result
  -v,--values-only         Output just values
  FILE                     Input file
```

TODO: Add example

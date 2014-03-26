# h2048

``` sh
# Update to latest version of Cabal.
cabal update
cabal install cabal-install

# Initialize a sandbox.
cabal sandbox init

# Install the package's dependencies.
cabal install --enable-benchmarks --enable-tests --flags=documentation --only-dependencies

# Configure & build the package.
cabal configure --enable-benchmarks --enable-tests
cabal build

# Test package.
cabal test

# Benchmark package.
cabal bench

# Run executable.
cabal run h2048

# Start REPL.
cabal repl lib:h2048

# Generate documentation.
cabal haddock --hyperlink-source

# Analyze coverage.
hpc markup --destdir=tmp dist/hpc/tix/hspec/hspec.tix
```

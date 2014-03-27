.PHONY: build configure haddock hpc install repl run test

all: install configure build haddock test hpc

build:
	cabal build

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi
	if test -e tmp/*.html; then rm tmp/*.html; fi

configure:
	cabal configure --enable-tests

format:
	git ls-files '*.hs' | xargs -n 1 scan --inplace-modify
	git ls-files '*.hs' | xargs stylish-haskell --inplace

haddock:
	cabal haddock --hyperlink-source
	# dist/doc/html/h2048/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/hspec/hspec.tix
	# tmp/hpc_index.html

install:
	cabal sandbox init
	cabal install --enable-tests --flags=documentation --only-dependencies

repl:
	cabal repl lib:h2048

run:
	cabal run h2048

test:
	cabal test

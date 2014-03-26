.PHONY: build configure haddock hpc install repl run test

all: install configure build haddock test hpc

build:
	cabal build

configure:
	cabal configure --enable-tests

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

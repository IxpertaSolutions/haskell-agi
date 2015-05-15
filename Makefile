.PHONY: all
all: .cabal-sandbox
	cabal build

.PHONY: clean
clean:
	cabal clean

.PHONY: distclean
distclean:
	cabal clean
	cabal sandbox delete

.PHONY: doc
doc: .cabal-sandbox
	cabal haddock

.cabal-sandbox:
	cabal sandbox init
	cabal install --dependencies-only

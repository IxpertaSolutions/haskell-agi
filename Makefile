.PHONY: all
all: dist
	cabal build

.PHONY: clean
clean:
	cabal clean

.PHONY: distclean
distclean:
	cabal clean
	cabal sandbox delete

dist:
	cabal sandbox init
	cabal install --dependencies-only


hoogle:
	hoogle server --local -p 8080

lint:
	@hlint .

tests:
	echo "Placeholder!"

# The following are only meant to be used during development:

configure:
	hpack && cabal configure

build:
	cabal build

.PHONY: hoogle lint configure build tests

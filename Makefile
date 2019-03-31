
build:
		@cabal build

configure:
		@hpack && cabal configure --enable-tests

lint:
		@hlint .

tests:
		@cabal test

hoogle:
		hoogle server --local -p 8080

continuous_tests:
		@ghcid --command="cabal repl x1-test"

.PHONY: hoogle lint configure build tests continuous_tests

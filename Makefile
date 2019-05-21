
build:
		@cabal new-build

configure:
		@hpack --force && cabal new-configure --enable-tests

repl:
		@cabal new-repl lib:x1

lint:
		@hlint .

tests:
		@cabal new-test

hoogle:
		hoogle server --local -p 8080

continuous_tests:
		@ghcid --command="cabal new-repl x1-test"

focus_check:
		@grep -Ern "(fit|fdescribe|fcontext|focus)" tests || exit 0 && exit 1

.PHONY: hoogle lint configure build repl tests continuous_tests focus_check

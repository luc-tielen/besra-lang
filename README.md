
# Besra

[![CircleCI](https://circleci.com/gh/luc-tielen/besra-lang.svg?style=svg&circle-token=07fcf633c70820100c529dda8869baa60d4b6dd8)](https://circleci.com/gh/luc-tielen/besra-lang)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/luc-tielen/besra-lang/blob/master/LICENSE)

This repository contains the code for the Besra programming language.
Besra is a statically typed, purely functional programming language with
a focus on correctness and performance.

Note: This is currently still an **extreme** WIP project, not usable at the moment.


## Getting started

The compiler is built using Cabal (for the actual building of the code) and Nix
(for creating a project-specific sandbox and managing dependencies/toolchain).
The easiest way to get started is as follows
([assuming Nix is installed](https://nixos.org/nix/download.html)):

```bash
$ git clone git@github.com:luc-tielen/besra-lang.git
$ cd besra-lang
$ nix-shell
$ cabal new-configure # done inside the nix-shell
$ cabal new-build     # done inside the nix-shell
```

The most often used commands are provided by a Makefile, so instead of
`cabal new-build` it is possible to also to use `make build`. For other
interesting commands, take a look at the
[Makefile](https://github.com/luc-tielen/besra-lang/blob/master/Makefile).

Because the project makes use of Nix, it is also very easy to lookup Haskell
docs using a local Hoogle server (start with `make Hoogle`). After the Hoogle
server is running, you can browse all available functions in the project.

The project is also setup to provide `ghcid` inside the Nix-shell, offering you
rapid feedback on compiler errors.

Some of the useful things you can do with ghcid are as follows:

```bash
# Monitor all files inside the lib folder for changes:
$ ghcid --command="cabal new-repl lib:besra"
# Monitor all files inside the src & lib folder for changes:
$ ghcid --command="cabal new-repl exe:besra"
# Monitor all files inside the src & tests folder for changes:
$ ghcid --command="cabal new-repl besra-test"
```

There are ofcourse many more possibilities when using ghcid. For a full guide
on how to use ghcid, please refer to the [ghcid
README](https://github.com/ndmitchell/ghcid).


## Contributing

Interested in contributing? Great! To get an idea if where you can contribute,
take a look at the
[list of open issues](https://github.com/luc-tielen/besra-lang/issues).


## Documentation

See the [docs folder](https://github.com/luc-tielen/besra-lang/tree/master/docs)
for more information.


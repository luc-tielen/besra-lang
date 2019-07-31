
# Architecture

This document describes the high level ideas used inside the compiler.
Note: some of these parts are still WIP.


## Project layout

There are 3 important folders in the project:

1. lib/
2. src/
3. tests/

Each of these folders is explained in the following subsections.


### lib/

This folder contains the main logic for the compiler. The compiler is built as
a library so that it can be easily tested since it only consists of functions
that can be composed together to form the entire compiler.
As an added benefit, this also makes it easy to reuse functions in a different
way (for example in a pretty-printer).

The parts of the compiler themselves are explained more in depth
[here](https://github.com/luc-tielen/besra-lang.git/docs/architecture.md#nanopass_compiler).


### src/

This folder contains files related to creating the final executable.
The files in this folder import files from `lib/` to expose the
functionality of the compiler to the outside world.

The `besra` executable is structured in such a way that the CLI interface
contains various subcommands:

- fmt (formatter)
- repl (interactive REPL)
- compiler
- ...

For example, the formatter can be invoked as: `besra fmt INPUT_FILE`.

Note: currently only the formatter part is functional until more parts of the
compiler are implemented.


### tests/

The tests folder contains all the tests / fixtures of the project.
Most tests import functions from `lib/` and run a set of
compiler-passes (see
[below](https://github.com/luc-tielen/besra-lang.git/docs/architecture.md#nanopass_compiler))
on a piece of Besra code, and check it against the resulting output.


## Nanopass compiler

The architecture of the compiler is best described as a "nanopass" compiler.
What this means is that the compiler consists of various small, focused parts
(so called `phases`). This is in contrast with older compilers that consisted of
1 big monolithic pass or a few (micro)-passes.

Each of these passes performs a transformation on the AST. Along the way, the
AST is rewritten multiple times and changes from a very rich tree structure to
a simpler structure containing only a few core primitive expressions.
This process repeats itself until the final Besra intermediate representation
(IR), which is then converted to LLVM IR. After this point, it is possible to
piggy-back on the LLVM framework to produce the final assembly code.


Summary of the IRs used in the code:

0. IR0: Raw input text, input for the compiler
1. IR1: the parsed input text
2. IR2: stripped down version of IR1, no longer has parser specific information
3. ...

The data types that together form an IR are grouped together in files named
`IR1.hs`, `IR2.hs`, ... for easy importing.


## Specific concepts

The following section explains some of the concepts used in the project.


### Annotations

For most algorithms in a compiler, information specific for each node needs to
be tracked. The way this is done in the Besra compiler is by making use of the
"trees that grow" approach.

In short, each AST node is decorated with a specific annotation type. The
annotations are specified by a type family, taking the compiler phase as input,
making it possible to change the type of the annotation during each compiler
phase. This allows us to add information to each AST node depending on the
algorithm.

The full explanation for this technique can be found in
[this paper](https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf).

The main file that implements this approach can be found
[here](https://github.com/luc-tielen/besra-lang/blob/master/lib/Besra/Types/Ann.hs).


### Parser combinators

The Besra parser is built using the parser combinators provided by the
[megaparsec](https://hackage.haskell.org/package/hspec-megaparsec) package.
Most modules related to parsing expose 1 top level function `parse` which can
then be used in other modules to continue composing parsers.


### Semantic analysis

Semantic analysis is performed after the input text is successfully parsed.
Every check during the analysis only computes a result and never performs a
modification to the AST. This makes it really easy to combine the results
together with a monoid data type (see `ValidationResult` in
[SA/Helpers.hs](https://github.com/luc-tielen/besra-lang/blob/master/lib/Besra/SA/Helpers.hs)).
Furthermore, the associative property of the monoid typeclass allows us
to run the analysis checks in parallel.


### Type system

TODO


### Optimizer

TODO


# Grammatical Framework Haskell Starter Project

This repository contains a simple Haskell project that uses the Grammatical Framework (GF) to generate natural language text. The project is set up to use the [PGF2 Haskell library](https://hackage.haskell.org/package/pgf2), which provides a Haskell interface to the GF runtime system.

## Dependencies

The project requires the following dependencies:

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- [Grammatical Framework](https://www.grammaticalframework.org/download/)
- [PGF2 Haskell library](https://hackage.haskell.org/package/pgf2)
- [GF C Runtime](https://github.com/GrammaticalFramework/gf-core/blob/master/src/runtime/c/INSTALL)

This project has only been tested on MacOS Sequoia 15.2 with Haskell Stack 3.3.1, Grammatical Framework 3.11, and PGF2 1.3.0, and GHC 8.10.7.

## Usage

The project comes with a simple example grammar which can be built using the `gf` command line tool to generate a PGF file.

```bash
gf -make Hello.gf HelloEng.gf HelloIta.gf
```

Build the stack project:

```bash
stack build
```

Run the project:

```bash
stack run
```

You should see the following output:

```plaintext
Available languages:
["HelloEng","HelloIta"]
Parse tree:
Hello World
Italian translation:
"ciao mondo"
```

## Tests

To run the tests, use the following command:

```bash
stack test
```

You should see the following output:

```plaintext
grammatical-framework-haskell-starter-project $ stack test
grammatical-framework-haskell-starter-project> test (suite: grammatical-framework-haskell-starter-project-test)

PGF Translation Tests
  should load PGF file successfully [✔]
  should have English language available [✔]
  should have Italian language available [✔]
  should parse English input correctly [✔]
  should translate from English to Italian [✔]
  should handle invalid input gracefully [✔]

Finished in 0.0049 seconds
6 examples, 0 failures
```

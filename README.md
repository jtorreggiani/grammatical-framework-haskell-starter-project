# Grammatical Framework Haskell Starter Project

This repository contains a simple Haskell project that uses the Grammatical Framework (GF) to generate natural language text. The project is set up to use the [PGF2 Haskell library](https://hackage.haskell.org/package/pgf2), which provides a Haskell interface to the GF runtime system.

## Dependencies

The project requires the following dependencies:

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- [Grammatical Framework](https://www.grammaticalframework.org/download/)
- [PGF2 Haskell library](https://hackage.haskell.org/package/pgf2)
- [GF C Runtime](https://github.com/GrammaticalFramework/gf-core/blob/master/src/runtime/c/INSTALL)

This project has only been tested on MacOS Sequoia 15.2 with Haskell Stack 3.3.1, Grammatical Framework 3.11, and PGF2 1.3.0, and GHC 8.10.7.

## Folder Structure

```
├── CHANGELOG.md
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── grammars
│   ├── Hello.gf
│   ├── Hello.pgf
│   ├── HelloEng.gf
│   └── HelloIta.gf
├── grammatical-framework-haskell-starter-project.cabal
├── package.yaml
├── src
│   └── Lib.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    └── Spec.hs
```

## Usage

The project comes with a simple example grammar which can be built using the `gf` command line tool to generate a PGF file.

```bash
cd grammars
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

The test/Spec.hs performs the same operations as the main function, but in a test suite to provide documentation and examples of how to use the PGF2 Haskell library.

```haskell
import Test.Hspec
import qualified Data.Map as Map
import PGF2

-- Helper function to load PGF file for tests
loadTestPGF :: IO PGF
loadTestPGF = readPGF "Hello.pgf"

main :: IO ()
main = hspec $ do
    describe "PGF Translation Tests" $ do
        it "should load PGF file successfully" $ do
            pgf <- loadTestPGF
            let hasLanguages = not . Map.null . languages $ pgf
            hasLanguages `shouldBe` True

        it "should have English language available" $ do
            pgf <- loadTestPGF
            Map.member "HelloEng" (languages pgf) `shouldBe` True

        it "should have Italian language available" $ do
            pgf <- loadTestPGF
            Map.member "HelloIta" (languages pgf) `shouldBe` True

        it "should parse English input correctly" $ do
            pgf <- loadTestPGF
            let eng = languages pgf Map.! "HelloEng"
            let parseResult = parse eng (startCat pgf) "hello world"
            case parseResult of
                ParseOk ((_,_):_) -> True `shouldBe` True
                _ -> expectationFailure "Failed to parse English input"

        it "should translate from English to Italian" $ do
            pgf <- loadTestPGF
            let eng = languages pgf Map.! "HelloEng"
            let ita = languages pgf Map.! "HelloIta"
            let parseResult = parse eng (startCat pgf) "hello world"
            case parseResult of
                ParseOk ((tree,_):_) -> do
                    let italianText = linearize ita tree
                    not (null italianText) `shouldBe` True
                _ -> expectationFailure "Failed to translate to Italian"

        it "should handle invalid input gracefully" $ do
            pgf <- loadTestPGF
            let eng = languages pgf Map.! "HelloEng"
            let parseResult = parse eng (startCat pgf) "invalid input that should fail"
            case parseResult of
                ParseOk _ -> expectationFailure "Should have failed on invalid input"
                ParseIncomplete -> expectationFailure "Should have failed completely, not incomplete"
                ParseFailed _ _ -> True `shouldBe` True
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

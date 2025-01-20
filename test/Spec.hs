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
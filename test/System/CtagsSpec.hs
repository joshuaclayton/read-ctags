module System.CtagsSpec where

import System.Ctags
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "System.Ctags" $
    it "finds tags" $ do
        Right tags <- tokensFromFile
        tags `shouldSatisfy` elem "tokensFromFile"
        tags `shouldSatisfy` elem "tokensFromStdin"
        tags `shouldNotSatisfy` elem "!_TAG_FILE_FORMAT"

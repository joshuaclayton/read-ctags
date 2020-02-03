module System.CtagsSpec where

import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BS
import qualified Data.Either as E
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import System.Ctags
import System.Ctags.IO (safeReadFile)
import System.Ctags.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "System.Ctags" $ do
        it "finds tags" $ do
            (Right tags) <- BF.second (fmap ctagName) <$> tokensFromFile
            tags `shouldSatisfy` elem "tokensFromFile"
            tags `shouldSatisfy` elem "tokensFromStdin"
            tags `shouldNotSatisfy` elem "!_TAG_FILE_FORMAT"
        it "handles only non-metadata" $ do
            let (Right successfulParse) =
                    parse
                        "!_TAG_FILE_FORMAT\t2\t/extended format; --format=1 will not append ;\" to lines/"
            length successfulParse `shouldBe` 0
        it "handles tag metadata" $ do
            (Right contents) <-
                BF.second lenientUtf8Decode <$>
                safeReadFile "./test/fixtures/constable.tags"
            length (E.fromRight [] $ parse contents) `shouldBe` 825
        it "handles no metadata" $ do
            let (Right successfulParse) =
                    parse
                        "A11YswitchCheck\t../assets/js/checkbox-switch.js\t/^const A11YswitchCheck = function() {$/"
            successfulParse `shouldBe`
                [ CtagItem
                      "A11YswitchCheck"
                      "../assets/js/checkbox-switch.js"
                      "/^const A11YswitchCheck = function() {$/"
                      []
                ]
        it "handles tag fields" $ do
            let (Right successfulParse) =
                    parse
                        "working_now?\t../app/models/person.rb\t/^  def working_now?$/;\"\tf\tclass:Person"
            successfulParse `shouldBe`
                [ CtagItem
                      "working_now?"
                      "../app/models/person.rb"
                      "/^  def working_now?$/"
                      [KindField Function, Field "class" "Person"]
                ]

lenientUtf8Decode :: BS.ByteString -> T.Text
lenientUtf8Decode = T.decodeUtf8With T.lenientDecode

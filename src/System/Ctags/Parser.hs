module System.Ctags.Parser
    ( parse
    ) where

import Control.Applicative ((<|>))
import qualified Data.Bifunctor as BF
import Data.Functor (($>))
import qualified Data.Maybe as M
import qualified Data.Text.Lazy as T
import Data.Void (Void)
import System.Ctags.Types
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Parser = M.Parsec Void T.Text

parse :: T.Text -> Either T.Text [CtagItem]
parse =
    BF.bimap (T.pack . M.errorBundlePretty) M.catMaybes .
    M.parse (M.sepBy lineParser M.newline <* M.eof) "" . T.strip

lineParser :: Parser (Maybe CtagItem)
lineParser =
    M.try metaDataParser <|> M.try ctagWithFieldsParser <|>
    ctagWithoutFieldsParser

metaDataParser :: Parser (Maybe CtagItem)
metaDataParser = M.string "!_TAG" *> toNewline $> Nothing <?> "metadata"

ctagWithoutFieldsParser :: Parser (Maybe CtagItem)
ctagWithoutFieldsParser = do
    tagName <- tagNameParser
    tagFile <- tagFileParser
    tagAddress <- tagAddressWithoutFieldsParser
    let language = calculateLanguageForFile tagFile
    return $ Just $ CtagItem tagName tagFile tagAddress language []

ctagWithFieldsParser :: Parser (Maybe CtagItem)
ctagWithFieldsParser = do
    tagName <- tagNameParser
    tagFile <- tagFileParser
    tagAddress <- tagAddressWithFieldsParser
    let language = calculateLanguageForFile tagFile
    tagFields <- tagFieldsParser language
    return $ Just $ CtagItem tagName tagFile tagAddress language tagFields

tagAddressWithFieldsParser :: Parser T.Text
tagAddressWithFieldsParser =
    T.pack <$> M.manyTill M.anySingle (M.string ";\"") <?> "tagAddress"

tagAddressWithoutFieldsParser :: Parser T.Text
tagAddressWithoutFieldsParser = T.pack <$> toNewline <?> "tagAddress"

tagNameParser :: Parser T.Text
tagNameParser = T.pack <$> toTab <?> "tagName"

tagFileParser :: Parser FilePath
tagFileParser = toTab <?> "tagFile"

toTab :: Parser String
toTab = untilParser '\t' <* M.tab

tagFieldsParser :: Maybe Language -> Parser [TagField]
tagFieldsParser l = M.tab *> M.sepBy (tagFieldParser l) M.tab

tagFieldParser :: Maybe Language -> Parser TagField
tagFieldParser l =
    M.try classFieldParser <|> M.try moduleFieldParser <|> M.try fieldParser <|>
    kindParser
  where
    kindParser = KindField <$> tokenKindParser l
    classFieldParser = ClassField <$> (M.string "class:" *> valueParser)
    moduleFieldParser = ModuleField <$> (M.string "module:" *> valueParser)
    valueParser = M.takeWhileP Nothing (\c -> c /= '\t' && c /= '\n')
    fieldParser =
        Field <$> (T.pack <$> M.many M.alphaNumChar <* M.char ':') <*>
        valueParser

tokenKindParser :: Maybe Language -> Parser TokenKind
tokenKindParser (Just language) = calculateKind language <$> M.letterChar
tokenKindParser Nothing = Unknown <$> M.letterChar

untilParser :: Char -> Parser String
untilParser c = M.some (M.satisfy (not . (==) c))

toNewline :: Parser String
toNewline = untilParser '\n'

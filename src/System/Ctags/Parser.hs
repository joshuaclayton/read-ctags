module System.Ctags.Parser
    ( parse
    ) where

import Control.Applicative ((<|>))
import qualified Data.Bifunctor as BF
import qualified Data.Char as C
import Data.Functor (($>))
import qualified Data.Maybe as M
import qualified Data.Text.Lazy as T
import Data.Void (Void)
import System.Ctags.Types
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Parser = M.Parsec Void String

data ParsedField
    = ParsedField !String
                  !String
    | KindField !TokenKind

parse :: T.Text -> Either T.Text [CtagItem]
parse =
    BF.bimap (T.pack . M.errorBundlePretty) M.catMaybes .
    M.parse (M.sepBy lineParser M.newline <* M.eof) "" . T.unpack . T.strip

lineParser :: Parser (Maybe CtagItem)
lineParser = metaDataParser <|> ctagWithFieldsParser

metaDataParser :: Parser (Maybe a)
metaDataParser = M.string "!_TAG" *> toNewline $> Nothing <?> "metadata"

ctagWithFieldsParser :: Parser (Maybe CtagItem)
ctagWithFieldsParser = do
    tagName <- tagNameParser
    tagFile <- tagFileParser
    let language = calculateLanguageForFile tagFile
    (tagAddress, tagFields) <- addressAndFieldsParser language
    return $
        Just $
        CtagItem
            tagName
            tagFile
            tagAddress
            language
            (M.fromMaybe Undefined $ tagKind tagFields)
            (M.catMaybes $ newField <$> tagFields)
  where
    newField (ParsedField "class" v) = Just $ ClassField v
    newField (ParsedField "module" v) = Just $ ModuleField v
    newField (ParsedField k v) = Just $ Field k v
    newField (KindField _) = Nothing
    safeHead (x:_) = Just x
    safeHead [] = Nothing
    fieldToKind (ParsedField _ _) = Nothing
    fieldToKind (KindField v) = Just v
    tagKind tagFields = safeHead $ M.catMaybes $ fieldToKind <$> tagFields

addressAndFieldsParser :: Maybe Language -> Parser (String, [ParsedField])
addressAndFieldsParser l =
    M.try ((,) <$> tagAddressWithFieldsParser <*> tagFieldsParser l) <|>
    ((,) <$> tagAddressWithoutFieldsParser <*> pure [])

tagAddressWithFieldsParser :: Parser String
tagAddressWithFieldsParser =
    M.manyTill (M.anySingleBut '\n') (M.string ";\"") <?> "tagAddress"

tagAddressWithoutFieldsParser :: Parser String
tagAddressWithoutFieldsParser = toNewline <?> "tagAddress"

tagNameParser :: Parser String
tagNameParser = toTab <?> "tagName"

tagFileParser :: Parser FilePath
tagFileParser = toTab <?> "tagFile"

toTab :: Parser String
toTab = untilParser '\t' <* M.tab

tagFieldsParser :: Maybe Language -> Parser [ParsedField]
tagFieldsParser l = M.tab *> M.sepBy (tagFieldParser l) M.tab

tagFieldParser :: Maybe Language -> Parser ParsedField
tagFieldParser l = M.try fieldParser <|> kindParser
  where
    kindParser = KindField <$> tokenKindParser l
    valueParser = M.takeWhileP Nothing (\c -> c /= '\t' && c /= '\n')
    fieldParser =
        ParsedField <$> (M.takeWhileP Nothing C.isAlphaNum <* M.char ':') <*>
        valueParser

tokenKindParser :: Maybe Language -> Parser TokenKind
tokenKindParser (Just language) = calculateKind language <$> M.letterChar
tokenKindParser Nothing = Unknown <$> M.letterChar

untilParser :: Char -> Parser String
untilParser c = M.takeWhileP Nothing (not . (==) c)

toNewline :: Parser String
toNewline = untilParser '\n'

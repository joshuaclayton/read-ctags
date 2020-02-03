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
ctagWithoutFieldsParser =
    Just <$>
    (CtagItem <$> tagNameParser <*> tagFileParser <*>
     tagAddressWithoutFieldsParser <*>
     pure [])

ctagWithFieldsParser :: Parser (Maybe CtagItem)
ctagWithFieldsParser =
    Just <$>
    (CtagItem <$> tagNameParser <*> tagFileParser <*> tagAddressWithFieldsParser <*>
     tagFieldsParser)

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

tagFieldsParser :: Parser [TagField]
tagFieldsParser = M.tab *> M.sepBy tagFieldParser M.tab

tagFieldParser :: Parser TagField
tagFieldParser = M.try fieldParser <|> kindParser
  where
    kindParser = KindField <$> tokenKindParser
    fieldParser =
        Field <$> (T.pack <$> M.many M.alphaNumChar <* M.char ':') <*>
        (T.pack <$> M.many M.alphaNumChar)

tokenKindParser :: Parser TokenKind
tokenKindParser =
    M.choice
        [ M.char 'c' $> Class
        , M.char 'd' $> Define
        , M.char 'e' $> Enumerator
        , M.char 'f' $> Function
        , M.char 'F' $> FileName
        , M.char 'g' $> EnumerationName
        , M.char 'm' $> Member
        , M.char 'p' $> FunctionPrototype
        , M.char 's' $> StructureName
        , M.char 't' $> Typedef
        , M.char 'u' $> UnionName
        , M.char 'v' $> Variable
        , Unknown <$> M.letterChar
        ]

untilParser :: Char -> Parser String
untilParser c = M.some (M.satisfy (not . (==) c))

toNewline :: Parser String
toNewline = untilParser '\n'

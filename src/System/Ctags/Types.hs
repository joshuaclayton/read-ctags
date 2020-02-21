module System.Ctags.Types
    ( TagSearchOutcome(..)
    , Language(..)
    , CtagItem(..)
    , TokenKind(..)
    , TagField(..)
    , calculateKind
    , calculateLanguageForFile
    ) where

import qualified Control.Exception as E
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as T
import qualified System.FilePath as FP

data TagSearchOutcome
    = TagsFileNotFound [String]
    | IOError E.IOException
    | UnableToParseTags T.Text
    deriving (Show)

data CtagItem = CtagItem
    { ctagName :: !String
    , ctagFile :: !FilePath
    , ctagAddress :: !String
    , ctagLanguage :: !(Maybe Language)
    , ctagKind :: !TokenKind
    , ctagFields :: ![TagField]
    } deriving (Eq, Show)

instance A.ToJSON CtagItem where
    toJSON ctagItem =
        A.object
            [ "name" .= ctagName ctagItem
            , "file" .= ctagFile ctagItem
            , "language" .= A.toJSON (ctagLanguage ctagItem)
            , "kind" .= A.toJSON (ctagKind ctagItem)
            , "tags" .= A.toJSON (ctagFields ctagItem)
            ]

data TagField
    = ClassField !String
    | ModuleField !String
    | Field !String
            !String
    deriving (Eq, Show)

instance A.ToJSON TagField where
    toJSON (ClassField t) = A.object ["class" .= t]
    toJSON (ModuleField t) = A.object ["module" .= t]
    toJSON (Field k v) = A.object [Text.pack k .= v]

instance A.ToJSON TokenKind where
    toJSON Undefined = A.Null
    toJSON (MissingLanguageToken _ _) = A.String "missing"
    toJSON (Unknown _) = A.String "unknown"
    toJSON v = A.String $ toToken v

data Language
    = CSS
    | Elixir
    | Elm
    | HTML
    | JSON
    | JavaScript
    | Markdown
    | Ruby
    | SCSS
    | Sh
    | SVG
    | TypeScript
    | XML
    deriving (Show, Eq)

instance A.ToJSON Language where
    toJSON = A.String . toToken

{-# INLINE toToken #-}
toToken :: Show a => a -> Text.Text
toToken = Text.pack . show

calculateLanguageForFile :: FilePath -> Maybe Language
calculateLanguageForFile = calculateLanguage . FP.takeExtension

calculateLanguage :: String -> Maybe Language
calculateLanguage ".css" = Just CSS
calculateLanguage ".ex" = Just Elixir
calculateLanguage ".exs" = Just Elixir
calculateLanguage ".elm" = Just Elm
calculateLanguage ".html" = Just HTML
calculateLanguage ".json" = Just JSON
calculateLanguage ".js" = Just JavaScript
calculateLanguage ".jsx" = Just JavaScript
calculateLanguage ".md" = Just Markdown
calculateLanguage ".rb" = Just Ruby
calculateLanguage ".scss" = Just SCSS
calculateLanguage ".svg" = Just SVG
calculateLanguage ".ts" = Just TypeScript
calculateLanguage ".tsx" = Just TypeScript
calculateLanguage ".xml" = Just XML
calculateLanguage "" = Just Sh
calculateLanguage _ = Nothing

calculateKind :: Language -> Char -> TokenKind
calculateKind CSS 'c' = Class
calculateKind CSS 'i' = Id
calculateKind CSS 's' = Selector
calculateKind Elixir 'a' = Macro
calculateKind Elixir 'c' = Callback
calculateKind Elixir 'd' = Delegate
calculateKind Elixir 'e' = Exception
calculateKind Elixir 'f' = Function
calculateKind Elixir 'g' = Guard
calculateKind Elixir 'i' = Implementation
calculateKind Elixir 'm' = Module
calculateKind Elixir 'o' = Operator
calculateKind Elixir 'p' = Protocol
calculateKind Elixir 'r' = Record
calculateKind Elixir 't' = Test
calculateKind Elixir 'y' = Type
calculateKind Elm 'a' = Alias
calculateKind Elm 'c' = Constructor
calculateKind Elm 'f' = Function
calculateKind Elm 'm' = Module
calculateKind Elm 'n' = Namespace
calculateKind Elm 'p' = Port
calculateKind Elm 't' = Type
calculateKind HTML 'C' = Stylesheet
calculateKind HTML 'I' = Id
calculateKind HTML 'J' = Script
calculateKind HTML 'a' = Anchor
calculateKind HTML 'c' = Class
calculateKind HTML 'h' = Heading1
calculateKind HTML 'i' = Heading2
calculateKind HTML 'j' = Heading3
calculateKind JSON 'a' = Array
calculateKind JSON 'b' = Boolean
calculateKind JSON 'n' = Number
calculateKind JSON 'o' = Object
calculateKind JSON 's' = String
calculateKind JSON 'z' = Null
calculateKind JavaScript 'C' = Constant
calculateKind JavaScript 'G' = Getter
calculateKind JavaScript 'S' = Setter
calculateKind JavaScript 'c' = Class
calculateKind JavaScript 'f' = Function
calculateKind JavaScript 'g' = Generator
calculateKind JavaScript 'm' = Method
calculateKind JavaScript 'p' = Property
calculateKind JavaScript 'v' = Variable
calculateKind Markdown 'S' = Subsection
calculateKind Markdown 'T' = L4Subsection
calculateKind Markdown 'c' = Chapter
calculateKind Markdown 's' = Section
calculateKind Markdown 't' = SubSubsection
calculateKind Markdown 'u' = L5Subsection
calculateKind Ruby 'S' = SingletonMethod
calculateKind Ruby 'c' = Class
calculateKind Ruby 'f' = Method
calculateKind Ruby 'm' = Module
calculateKind SCSS 'P' = Placeholder
calculateKind SCSS 'c' = Class
calculateKind SCSS 'f' = Function
calculateKind SCSS 'i' = Id
calculateKind SCSS 'm' = Mixin
calculateKind SCSS 'v' = Variable
calculateKind SCSS 'z' = Parameter
calculateKind Sh 'a' = Alias
calculateKind Sh 'f' = Function
calculateKind Sh 'h' = Heredoc
calculateKind Sh 's' = Script
calculateKind TypeScript 'C' = Constant
calculateKind TypeScript 'G' = Generator
calculateKind TypeScript 'a' = Alias
calculateKind TypeScript 'c' = Class
calculateKind TypeScript 'e' = Enumerator
calculateKind TypeScript 'f' = Function
calculateKind TypeScript 'g' = Enum
calculateKind TypeScript 'i' = Interface
calculateKind TypeScript 'l' = Local
calculateKind TypeScript 'm' = Method
calculateKind TypeScript 'n' = Namespace
calculateKind TypeScript 'p' = Property
calculateKind TypeScript 'v' = Variable
calculateKind TypeScript 'z' = Parameter
calculateKind XML 'i' = Id
calculateKind XML 'n' = NSPrefix
calculateKind XML 'r' = Root
calculateKind SVG 'd' = Def
calculateKind SVG v = calculateKind XML v
calculateKind l c = MissingLanguageToken l c

data TokenKind
 -- CSS
    = Class
    | Id
    | Selector
 -- Elixir
    | Macro
    | Callback
    | Delegate
    | Exception
    | Function
    | Guard
    | Implementation
    | Module
    | Operator
    | Protocol
    | Record
    | Test
    | Type
 -- Elm
    | Alias
    | Constructor
    | Namespace
    | Port
 -- HTML
    | Stylesheet
    | Script
    | Anchor
    | Heading1
    | Heading2
    | Heading3
 -- JSON
    | Array
    | Boolean
    | Number
    | Object
    | String
    | Null
 -- JavaScript
    | Constant
    | Getter
    | Setter
    | Generator
    | Method
    | Property
    | Variable
 -- Markdown
    | Subsection
    | L4Subsection
    | Chapter
    | Section
    | SubSubsection
    | L5Subsection
 -- Ruby
    | SingletonMethod
 -- SCSS
    | Placeholder
    | Mixin
    | Parameter
 -- Sh
    | Heredoc
 -- SVG
    | Def
 -- TypeScript
    | Enumerator
    | Enum
    | Interface
    | Local
  -- XML
    | NSPrefix
    | Root
    | Undefined
    | MissingLanguageToken !Language
                           !Char
    | Unknown Char
    deriving (Eq, Show)

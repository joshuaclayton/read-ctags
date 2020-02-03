{-# LANGUAGE DeriveGeneric #-}

module System.Ctags.Types
    ( TagSearchOutcome(..)
    , CtagItem(..)
    , TokenKind(..)
    , TagField(..)
    ) where

import qualified Control.Exception as E
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as T
import GHC.Generics (Generic)

data TagSearchOutcome
    = TagsFileNotFound [String]
    | IOError E.IOException
    | UnableToParseTags T.Text
    deriving (Show)

data CtagItem = CtagItem
    { ctagName :: T.Text
    , ctagFile :: FilePath
    , ctagAddress :: T.Text
    , ctagFields :: [TagField]
    } deriving (Eq, Show, Generic)

instance A.ToJSON CtagItem

data TagField
    = KindField TokenKind
    | Field T.Text
            T.Text
    deriving (Eq, Show, Generic)

instance A.ToJSON TagField

instance A.ToJSON TokenKind

data TokenKind
    = Class
    | Define
    | Enumerator
    | Function
    | FileName
    | EnumerationName
    | Member
    | FunctionPrototype
    | StructureName
    | Typedef
    | UnionName
    | Variable
    | Undefined
    | Unknown Char
    deriving (Eq, Show, Generic)

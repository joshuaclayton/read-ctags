module System.Ctags.Types
    ( TagSearchOutcome(..)
    ) where

import qualified Control.Exception as E

data TagSearchOutcome
    = TagsFileNotFound [String]
    | IOError E.IOException

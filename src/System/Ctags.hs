module System.Ctags
    ( TagSearchOutcome(..)
    , CtagItem(..)
    , tokensFromFile
    , tokensFromStdin
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Bifunctor as BF
import System.Ctags.Internal
import qualified System.Ctags.Parser as P
import System.Ctags.Types

tokensFromFile :: MonadIO m => m (Either TagSearchOutcome [CtagItem])
tokensFromFile = (BF.first UnableToParseTags . P.parse =<<) <$> tagsContent

tokensFromStdin :: MonadIO m => m (Either TagSearchOutcome [CtagItem])
tokensFromStdin = BF.first UnableToParseTags . P.parse <$> stdinContent

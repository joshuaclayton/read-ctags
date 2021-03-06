module System.Ctags
    ( TagSearchOutcome(..)
    , CtagItem(..)
    , CtagsSearchConfig
    , tokensFromFile
    , tokensFromFile'
    , tokensFromStdin
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Bifunctor as BF
import System.Ctags.Internal
import qualified System.Ctags.Parser as P
import System.Ctags.Types

tokensFromFile :: MonadIO m => m (Either TagSearchOutcome [CtagItem])
tokensFromFile =
    (BF.first UnableToParseTags . P.parse =<<) <$>
    tagsContent defaultCtagsSearchConfig

tokensFromFile' ::
       MonadIO m => CtagsSearchConfig -> m (Either TagSearchOutcome [CtagItem])
tokensFromFile' config =
    (BF.first UnableToParseTags . P.parse =<<) <$> tagsContent config

tokensFromStdin :: MonadIO m => m (Either TagSearchOutcome [CtagItem])
tokensFromStdin = BF.first UnableToParseTags . P.parse <$> stdinContent

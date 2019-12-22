module System.Ctags
    ( TagSearchOutcome(..)
    , tokensFromFile
    , tokensFromStdin
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text.Lazy as T
import System.Ctags.Internal
import System.Ctags.Types

tokensFromFile :: MonadIO m => m (Either TagSearchOutcome [T.Text])
tokensFromFile = fmap tokensFromTags <$> tagsContent

tokensFromStdin :: MonadIO m => m (Either TagSearchOutcome [T.Text])
tokensFromStdin = Right . tokensFromTags <$> stdinContent

tokensFromTags :: T.Text -> [T.Text]
tokensFromTags = filter validTokens . uniqueList . tokenLocations . T.lines
  where
    tokenLocations = map (head . T.splitOn "\t")

validTokens :: T.Text -> Bool
validTokens = not . T.isPrefixOf "!_TAG"

module System.Ctags.Internal
    ( uniqueList
    , tagsContent
    , stdinContent
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import System.Ctags.IO
import System.Ctags.Types
import qualified System.Directory as D

uniqueList :: Ord a => [a] -> [a]
uniqueList = Set.toList . Set.fromList

tagsContent :: MonadIO m => m (Either TagSearchOutcome T.Text)
tagsContent = readTagsFile =<< findTagsFile

stdinContent :: MonadIO m => m T.Text
stdinContent = lenientUtf8Decode <$> getContentsLazy

findTagsFile :: MonadIO m => m (Maybe FilePath)
findTagsFile = findFile possibleTagsFileDirectories "tags"
  where
    findFile dirs = liftIO . D.findFile dirs

readTagsFile :: MonadIO m => Maybe String -> m (Either TagSearchOutcome T.Text)
readTagsFile Nothing =
    return $ Left $ TagsFileNotFound possibleTagsFileDirectories
readTagsFile (Just path) =
    BF.bimap IOError lenientUtf8Decode <$> safeReadFile path

possibleTagsFileDirectories :: [String]
possibleTagsFileDirectories = [".git", "tmp", "."]

lenientUtf8Decode :: BS.ByteString -> T.Text
lenientUtf8Decode = T.decodeUtf8With T.lenientDecode

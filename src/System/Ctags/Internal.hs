{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module System.Ctags.Internal
    ( CtagsSearchConfig
    , defaultCtagsSearchConfig
    , stdinContent
    , tagsContent
    ) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import System.Ctags.IO
import System.Ctags.Types
import qualified System.Directory as D

type AppConfig = MonadReader CtagsSearchConfig

newtype App a = App
    { runApp :: ReaderT CtagsSearchConfig (ExceptT TagSearchOutcome IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , AppConfig
               , MonadIO
               , MonadError TagSearchOutcome
               )

data CtagsSearchConfig = CtagsSearchConfig
    { tagFileName :: String
    , tagFileDirectories :: [String]
    }

defaultCtagsSearchConfig :: CtagsSearchConfig
defaultCtagsSearchConfig = CtagsSearchConfig "tags" [".git", "tmp", "."]

tagsContent ::
       MonadIO m => CtagsSearchConfig -> m (Either TagSearchOutcome T.Text)
tagsContent config = liftIO $ runExceptT (runReaderT (runApp run) config)

run :: App T.Text
run = readTagsFile =<< findTagsFile

stdinContent :: MonadIO m => m T.Text
stdinContent = lenientUtf8Decode <$> getContentsLazy

findTagsFile :: App FilePath
findTagsFile = do
    directories <- asks tagFileDirectories
    filename <- asks tagFileName
    maybe (throwError $ TagsFileNotFound directories) return =<<
        liftIO (D.findFile directories filename)

readTagsFile ::
       (MonadIO m, MonadError TagSearchOutcome m) => FilePath -> m T.Text
readTagsFile path =
    either (throwError . IOError) (return . lenientUtf8Decode) =<<
    safeReadFile path

lenientUtf8Decode :: BS.ByteString -> T.Text
lenientUtf8Decode = T.decodeUtf8With T.lenientDecode

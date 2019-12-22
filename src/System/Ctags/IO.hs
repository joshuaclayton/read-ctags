module System.Ctags.IO
    ( safeReadFile
    , getContentsLazy
    ) where

import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as BS

safeReadFile :: MonadIO m => FilePath -> m (Either E.IOException BS.ByteString)
safeReadFile filePath = liftIO $ E.try $ BS.readFile filePath

getContentsLazy :: MonadIO m => m BS.ByteString
getContentsLazy = liftIO BS.getContents

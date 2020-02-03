module Main
    ( main
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Options.Applicative as OA
import System.Ctags as Ctags
import qualified System.IO as SIO

newtype CtagsFromStdin =
    CtagsFromStdin Bool

main :: MonadIO m => m ()
main = runProgram =<< parseCLI

runProgram :: MonadIO m => CtagsFromStdin -> m ()
runProgram (CtagsFromStdin o) = either handleFailure handleSuccess =<< loadTags
  where
    handleSuccess = liftIO . BS.putStr . A.encode
    handleFailure = toStdErr . T.pack . renderError
    loadTags =
        if o
            then Ctags.tokensFromStdin
            else Ctags.tokensFromFile

toStdErr :: MonadIO m => T.Text -> m ()
toStdErr = hPutStrLazy SIO.stderr

hPutStrLazy :: MonadIO m => SIO.Handle -> T.Text -> m ()
hPutStrLazy h = liftIO . BS.hPutStr h . T.encodeUtf8

renderError :: Ctags.TagSearchOutcome -> String
renderError (TagsFileNotFound _) = "Unable to find tags file"
renderError (IOError e) = show e
renderError (UnableToParseTags e) = show e

parseCLI :: MonadIO m => m CtagsFromStdin
parseCLI =
    liftIO $ OA.execParser (withInfo parseOptions pHeader pDescription pFooter)
  where
    pHeader = "read-ctags"
    pDescription =
        "read-ctags reads available tokens from a ctags file\
                   \(located at .git/tags, ./tags, or tmp/tags)"
    pFooter = "CLI USAGE: $ read-ctags"

withInfo :: OA.Parser a -> String -> String -> String -> OA.ParserInfo a
withInfo opts h d f =
    OA.info (OA.helper <*> opts) $ OA.header h <> OA.progDesc d <> OA.footer f

parseOptions :: OA.Parser CtagsFromStdin
parseOptions = CtagsFromStdin <$> parseFromStdIn

parseFromStdIn :: OA.Parser Bool
parseFromStdIn = OA.switch $ OA.long "stdin" <> OA.help "Read tags from STDIN"

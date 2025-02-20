module Echidna.Utility where

import Control.Monad (unless)
import Control.Monad.Catch (bracket)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import System.Directory (getDirectoryContents, getCurrentDirectory, setCurrentDirectory)
import System.IO (hFlush, stdout)

measureIO :: Bool -> String -> IO b -> IO b
measureIO quiet message action = do
  unless quiet $ do
    time <- timePrefix
    putStr (time <> message  <> "... ") >> hFlush stdout
  t0 <- getCurrentTime
  ret <- action
  t1 <- getCurrentTime
  unless quiet $ putStrLn $ "Done! (" <> show (diffUTCTime t1 t0) <> ")"
  pure ret

timePrefix :: IO String
timePrefix = do
  time <- utcToLocalZonedTime =<< getCurrentTime
  pure $ "[" <> formatTime defaultTimeLocale "%F %T.%2q" time <> "] "

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

withCurrentDirectory
  :: FilePath  -- ^ Directory to execute in
  -> IO a      -- ^ Action to be executed
  -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory dir
    action

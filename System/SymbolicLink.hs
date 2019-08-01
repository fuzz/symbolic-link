module System.SymbolicLink
( filePathExist
) where

import Control.Exception
import System.Posix.Files (FileStatus, getSymbolicLinkStatus)

filePathExist :: FilePath -> IO Bool
filePathExist fp = do
    x <- try (getSymbolicLinkStatus fp) :: IO (Either IOError FileStatus)
    case x of
        Left _  -> return False
        Right _ -> return True

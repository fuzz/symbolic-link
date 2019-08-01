{-|
Module      : System.SymbolicLink
Description : Tools for working with symbolic links.
Copyright   : (c) Fuzz Leonaard, 2019
License     : BSD-3
Maintainer  : fuzz@kt-22.com
Stability   : experimental
Portability : POSIX

SymbolicLink provides tools for working with symbolic links on POSIX systems.

The executable @symlink@ changes to the user's home directory, reads in a
sequence of source/target mappings from a YAML file in @.symlinks@ and attempts
to create them. If the target exists and is a symbolic link it will be removed
and replaced, otherwise symlink will refuse to clobber it.

The function @filePathExist@ works like @fileExist@ but does
not follow the symlink, thus making it suitable for working with unreferenced
symlinks. Unreferenced symlinks are not necessarily "broken"; one should not
have to handle exceptions to work with them.
-}
module System.SymbolicLink
( filePathExist
) where

import Control.Exception
import System.Posix.Files (FileStatus, getSymbolicLinkStatus)

-- | 'filePathExist' works like 'System.Posix.Files.fileExist'
--   except it doesn't follow symlinks.
filePathExist :: FilePath -> IO Bool
filePathExist fp = do
    x <- try (getSymbolicLinkStatus fp) :: IO (Either IOError FileStatus)
    case x of
        Left _  -> return False
        Right _ -> return True

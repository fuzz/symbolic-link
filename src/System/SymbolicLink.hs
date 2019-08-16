{-|
Module      : System.SymbolicLink
Description : Tools for working with symbolic links.
Copyright   : (c) Fuzz Leonard, 2019
License     : BSD-3
Maintainer  : fuzz@kt-22.com
Stability   : experimental
Portability : POSIX

SymbolicLink provides tools for working with symbolic links on POSIX systems.

The executable @symlink@ changes to the user's home directory, reads in a
sequence of source/target mappings from a YAML file in @.symlinks@ and attempts
to create them. If the target exists and is a symbolic link it will be removed
and replaced, otherwise symlink will refuse to clobber it.

The function @filePathExist@ works like @fileExist@ from @System.Posix.Files@
or @doesPathExist@ from @System.Directory@ but does not follow symlinks, thus
making it suitable for working with unreferenced symlinks. Unreferenced
symlinks are not necessarily "broken"; one should not have to handle exceptions
to work with them.

Conceptually @filePathExist@ is concerned with the perspective of the current
user environment, thus we consider the path to not exist if the user does not
have the necessary permissions or if any other error occurs while attempting
to get the @FilePath@ status. 
-}

module System.SymbolicLink
  ( FileType
  , filePathExist
  , filePathExistEither
  , fileType
  , fileTypeEither
  , fileTypeMaybe
  , getFileTypeFromStatus
  )
where

import Control.Exception
import System.Posix.Files ( FileStatus
                          , getSymbolicLinkStatus
                          , isBlockDevice
                          , isCharacterDevice
                          , isDirectory
                          , isNamedPipe
                          , isRegularFile
                          , isSocket
                          , isSymbolicLink
                          )
-- | 'filePathExist' works like 'System.Posix.Files.fileExist'
--   except it doesn't follow symlinks.
filePathExist :: FilePath -> IO Bool
filePathExist f = do
  x <- try (getSymbolicLinkStatus f) :: IO (Either IOError FileStatus)
  case x of
    Left  _ -> return False
    Right _ -> return True

-- | Like 'filePathExist' but get the 'IOError' instead of 'False'
filePathExistEither :: FilePath -> IO (Either IOError Bool)
filePathExistEither f = do
  x <- try (getSymbolicLinkStatus f) :: IO (Either IOError FileStatus)
  case x of
    Left  e -> return $ Left e
    Right _ -> return $ Right True

-- | Return the actual 'FileType' of a 'FilePath' without following
-- symbolic links. This is unsafe; you should ensure you can get a
-- 'FileStatus' before evaluation.
fileType :: FilePath -> IO FileType
fileType f = do
  s <- getSymbolicLinkStatus f
  return $ getFileTypeFromStatus s

-- | Like 'fileType' but with errors wrapped for your protection.
fileTypeEither :: FilePath -> IO (Either IOError FileType)
fileTypeEither f = do
  x <- filePathExistEither f
  case x of
    Left  e -> return $ Left e
    Right _ -> do
      t <- fileType f
      return $ Right t

-- | Like 'fileType' without errors because life is short.
fileTypeMaybe :: FilePath -> IO (Maybe FileType)
fileTypeMaybe f = do
  x <- filePathExist f
  if x
    then do
      s <- getSymbolicLinkStatus f
      return $ Just (getFileTypeFromStatus s)
    else return Nothing

-- | Remember that 'FileStatus' can go stale and thus so can 'FileType'.
getFileTypeFromStatus :: FileStatus -> FileType
getFileTypeFromStatus s | isBlockDevice s  = BlockDevice
                        | isDirectory s    = Directory
                        | isNamedPipe s    = NamedPipe
                        | isRegularFile s  = RegularFile
                        | isSocket s       = Socket
                        | isSymbolicLink s = SymbolicLink
                        | otherwise        = Unknown

data FileType =
    BlockDevice
  | CharacterDevice
  | Directory
  | NamedPipe
  | RegularFile
  | Socket
  | SymbolicLink
  | Unknown
  deriving (Eq, Show)

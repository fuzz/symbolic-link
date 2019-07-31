{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Exception
import Data.Yaml          (FromJSON,
                           ParseException,
                           decodeFileEither)
import GHC.Generics
import System.Directory   (getHomeDirectory,
                           setCurrentDirectory)
import System.Posix.Files (FileStatus,
                           createSymbolicLink,
                           getSymbolicLinkStatus,
                           isSymbolicLink,
                           removeLink)

main = do
    h <- getHomeDirectory
    setCurrentDirectory h

    d <- decodeFileEither ".symlinks" :: IO (Either ParseException [LinkPair])

    case d of
        Left _ -> putStr "\nConfig file error--malformed YAML?\n"
        Right lps -> do
            sequence $ [makeLink lp | lp <- lps]
            putStr "You have been LINKED!\n"

makeLink :: LinkPair -> IO ()
makeLink lp = do
    x <- filePathExist $ target lp
    if x == True
        then clobberIfSymbolicLink lp
        else createSymbolicLink (source lp) (target lp)

clobberIfSymbolicLink :: LinkPair -> IO ()
clobberIfSymbolicLink lp = do
    s <- getSymbolicLinkStatus $ target lp
    if isSymbolicLink s == True
        then do
            removeLink $ target lp
            createSymbolicLink (source lp) (target lp)
        else putStr $ "Refusing to clobber non-symlink " ++ target lp ++ "\n"

filePathExist :: FilePath -> IO Bool
filePathExist fp = do
    x <- try (getSymbolicLinkStatus fp) :: IO (Either IOError FileStatus)
    case x of
        Left _  -> return False
        Right _ -> return True

data LinkPair = LinkPair {
      source :: FilePath
    , target :: FilePath
} deriving (Generic, Show)

instance FromJSON LinkPair
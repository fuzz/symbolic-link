import System.SymbolicLink
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain $
  testCase "FilePath should exist" $ do
    filePathExist "/Users/fuzz/testfile" @? "FilePath does not exist"

  testCase "fileType should recognize a regular file" $ do
    fileType "/Users/fuzz/testfile" (@?=) Just RegularFile

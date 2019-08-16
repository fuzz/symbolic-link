import System.Posix.Files
import System.SymbolicLink
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain $
  testCase "FilePath should exist" $ do
    let testFile = "/tmp/regular.file"
    writeFile testFile "test -- delete me"

    filePathExist testFile @? "FilePath does not exist"

    removeLink testFile

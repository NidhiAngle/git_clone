module ObjectTests where

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
  classify,  maxSuccess, listOf, resize, scale, (==>))
import ObjectStore as OS 
import Objects as O
import Data.ByteString.Char8 as C

commit = makeCommit [(C.pack "PARENT1"), (C.pack "PARENT2")] (C.pack "TREE") (C.pack "AUTHOR") (C.pack "MSG")
tree = makeTree [("blob", (C.pack "BLOB1"), (C.pack "first")), ("blob", (C.pack "BLOB2"), (C.pack "sec")),("tree", (C.pack "TREE"), (C.pack "third"))]
emptyTree = makeTree []
emptyParentCommit = makeCommit [] (C.pack "TREE") (C.pack "AUTHOR") (C.pack "MSG")
blob = makeBlob $ C.pack "Hi there everyone! Welcome to our git clone."

commitStr = C.pack "commit xx \0parent PARENT1\nparent PARENT2\ntree TREE\nauthor AUTHOR\n\nmsg MSG\n"
commitWOParentStr = C.pack "commit xx \0tree TREE\nauthor AUTHOR\n\nmsg MSG\n"
treeStr   = C.pack "tree xx \0blob BLOB1 first\nblob BLOB2 sec\ntree TREE third\n"
emptytreeStr = C.pack "tree xx\0"
blobStr   = C.pack "blob xx\0Hi there everyone! Welcome to our git clone."

-- exportObject :: Monad m => Repo -> O.Object -> (m FilePath,m FilePath, m C.ByteString)
--importObject :: Monad m => m ByteString -> m (Maybe O.Object)


importObjectTests = TestList [
  readObject commitStr ~?= Just (O.CommitObj commit),
  readObject commitWOParentStr ~?= Just (O.CommitObj emptyParentCommit),
  readObject treeStr ~?= Just (O.TreeObj tree),
  readObject emptytreeStr ~?= Just (O.TreeObj emptyTree),
  readObject blobStr ~?= Just (O.BlobObj blob)]
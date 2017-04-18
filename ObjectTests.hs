module ObjectTests where

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
  classify,  maxSuccess, listOf, resize, scale, (==>))
import ObjectStore as OS 
import Objects as O
import Data.ByteString.Char8 as C

commit = makeCommit [C.pack "PARENT ID"] (C.pack "TREE ID") (C.pack "AUTHOR") (C.pack "MSG")
tree = makeTree [("blob", (C.pack "BLOB1 ID"), (C.pack "First one")), ("blob", (C.pack "BLOB2 ID"), (C.pack "Second one")),("tree", (C.pack "Tree ID"), (C.pack "Second one"))]
emptyTree = makeTree []
emptyParentCommit = makeCommit [] (C.pack "TREE ID") (C.pack "AUTHOR") (C.pack "MSG")
blob = makeBlob $ C.pack "Hi there everyone! Welcome to our git clone."

commitStr= C.pack "parent PARENT1\nparent PARENT2\ntree TREE\nauthor AUTHOR\n\nmsg MSG\n"
commitWOParentStr = C.pack "tree TREE\nauthor AUTHOR\n\nmsg MS\n"
treeStr = C.pack "blob BLOB1 first\nblob BLOB2 sec\ntree TREE third\n"
emptytreeStr = C.pack ""
blobStr = C.pack "Hi there everyone! Welcome to our git clone."
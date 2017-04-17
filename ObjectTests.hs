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
emptyTree = []
emptyParentCommit = makeCommit [] (C.pack "TREE ID") (C.pack "AUTHOR") (C.pack "MSG")
blob = makeBlob $ C.pack "Hi there everyone! Welcome to our git clone."

commitStr= "parent PARENT1\nparent PARENT2\ntree TREE\nauthor AUTHOR\n\nmsg MSG"
commitWOParentStr = "tree TREE\nauthor AUTHOR\n\nmsg MSG"
treeStr = "blob BLOB1 first\nblob BLOB2 sec\ntree TREE third"
emptytreeStr = ""
blobStr = "Hi there everyone! Welcome to our git clone."
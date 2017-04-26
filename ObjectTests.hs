module ObjectTests where

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
  classify,  maxSuccess, listOf, resize, scale, (==>))
import ObjectStore as OS 
import Objects as O
import RepoMonad as RM
import Data.ByteString.Char8 as C
import qualified Data.Time.Clock as DT
import MyDiff as D 
import Control.Monad.Trans.Reader
import Control.Monad
import Control.Monad.Except


commit1 = makeCommit [(C.pack "PARENT1"), (C.pack "PARENT2")] (C.pack "TREE") (C.pack "AUTHOR") (C.pack "MSG") ((read "2011-11-19 18:28:52 UTC")::DT.UTCTime)
tree1 = makeTree (C.pack "tree1") [(O.makeBlobEntryType, (C.pack "BLOB1"), (C.pack "first")), (O.makeBlobEntryType, (C.pack "BLOB2")
                ,(C.pack "sec")),(O.makeTreeEntryType, (C.pack "TREE"), (C.pack "third"))]
emptyTree = makeTree (C.pack "emptyTree") []
emptyParentCommit = makeCommit [] (C.pack "TREE") (C.pack "AUTHOR") (C.pack "MSG") ((read "2011-11-19 18:28:52 UTC")::DT.UTCTime)
blob1 = makeBlob $ C.pack "Hi there everyone! Welcome to our git clone."

commitStr = C.pack "commit 75\0parent PARENT1\nparent PARENT2\ntree TREE\nauthor AUTHOR\n\nMSG\ntime 1321727332\n"
commitWOParentStr = C.pack "commit 45\0tree TREE\nauthor AUTHOR\n\nMSG\ntime 1321727332\n"
treeStr   = C.pack "tree 48\0blob BLOB1 first\nblob BLOB2 sec\ntree TREE third\n"
emptytreeStr = C.pack "tree 0\0"
blobStr   = C.pack "blob 44\0Hi there everyone! Welcome to our git clone."

blob2 = makeBlob $ C.pack "LINE1\nLINE2"
blob3 = makeBlob $ C.pack "LINE1\nLINE2"
blob4 = makeBlob $ C.pack "LINE1\nLINECHANGED2"
blob5 = makeBlob $ C.pack "LINE1"
blob6 = makeBlob $ C.pack "LINE1\nLINE2\nLINE3"

-- tree1 = makeTree $ [(O.makeTreeEntryType), ]
-- exportObject :: Monad m => Repo -> O.Object -> (m FilePath,m FilePath, m C.ByteString)
third :: (FilePath,O.ObjectId, C.ByteString) -> C.ByteString
third (a,b,c) = c

beforeTests = do
  -- RM.writeObjectToFile (BlobObj blob2)
  -- RM.writeObjectToFile (BlobObj blob3)
  -- RM.writeObjectToFile (BlobObj blob4)
  -- RM.writeObjectToFile (BlobObj blob5)
  -- RM.writeObjectToFile (BlobObj blob6)
  let 
    (i2,a) = OS.hashContent blob2
    (i3,b) = OS.hashContent blob3
    (i4,c) = OS.hashContent blob4
    (i5,d) = OS.hashContent blob5
    (i6,e) = OS.hashContent blob6
    tree2  = makeTree (C.pack "tree2") [(O.makeBlobEntryType, i2, C.pack "blob2")]
    tree4  = makeTree (C.pack "tree4") [(O.makeBlobEntryType, i4, C.pack "blob2")]
    tree3  = makeTree (C.pack "tree3") [(O.makeBlobEntryType, i2, C.pack "alsoblob2")]
    (t2,g) = OS.hashContent tree2
    (t4,h) = OS.hashContent tree4
    tree5  = makeTree (C.pack "tree5") [(O.makeBlobEntryType, i2, C.pack "blob2"),
                         (O.makeBlobEntryType, i3, C.pack "blob5"),
                         (O.makeBlobEntryType, i5, C.pack "blob4"),
                         (O.makeTreeEntryType, t2, C.pack "tree2")]
    
    tree6  = makeTree (C.pack "tree6") [(O.makeBlobEntryType, i2, C.pack "blob2"),
                         (O.makeBlobEntryType, i3, C.pack "blob3"),
                         (O.makeBlobEntryType, i4, C.pack "blob4"),
                         (O.makeTreeEntryType, t4, C.pack "tree2"),
                         (O.makeTreeEntryType, t2, C.pack "treex")]
--  return ()
    x = (~?=) <$> (getDiff (diff blob2 blob3)) <*> return "\nBlob 1:be2fe\nBlob 2:be2fe\n"
  _ <- (x >>= runTestTT)
  return ()


getDiff :: RepoState String -> IO String
getDiff x = do
  res <- runExceptT (runReaderT x "./") 
  case res of
    Right s -> return s
    Left  s -> return s

importObjectTests = TestList [
  readObject commitStr ~?= Just commit1,
  readObject commitWOParentStr ~?= Just emptyParentCommit,
  readObject treeStr ~?= Just tree1,
  readObject emptytreeStr ~?= Just emptyTree,
  readObject blobStr ~?= Just blob1]

exportObjectTests = TestList [
  third (exportObject "t" commit1) ~?= commitStr,
  third (exportObject "t" emptyParentCommit) ~?= commitWOParentStr,
  third (exportObject "t" tree1) ~?= treeStr,
  third (exportObject "t" emptyTree) ~?= emptytreeStr,
  third (exportObject "t" blob1) ~?= blobStr]
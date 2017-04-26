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
treeStr   = C.pack "tree 59\NULname tree1\nblob BLOB1 first\nblob BLOB2 sec\ntree TREE third\n"
emptytreeStr = C.pack "tree 15\NULname emptyTree\n"
blobStr   = C.pack "blob 44\0Hi there everyone! Welcome to our git clone."

blob2 = makeBlob $ C.pack "LINE1"
blob3 = makeBlob $ C.pack "LINE1"
blob4 = makeBlob $ C.pack "LINECHANGED2"
blob5 = makeBlob $ C.pack "LINE1"
blob6 = makeBlob $ C.pack "LINE1\nLINE3"

-- tree1 = makeTree $ [(O.makeTreeEntryType), ]
-- exportObject :: Monad m => Repo -> O.Object -> (m FilePath,m FilePath, m C.ByteString)
third :: (FilePath,O.ObjectId, C.ByteString) -> C.ByteString
third (a,b,c) = c

--beforeTests

finalTest = do
  _ <- runExceptT (runReaderT (RM.writeObjectToFile blob2 :: RepoState O.ObjectId) "./test/")
  _ <- runExceptT (runReaderT (RM.writeObjectToFile blob4 :: RepoState O.ObjectId) "./test/")
  
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
  
    x = [(~?=) <$> (getDiff (diff blob2 blob3)) <*> return s1,
         (~?=) <$> (getDiff (diff blob2 blob4)) <*> return s2,
         (~?=) <$> (getDiff (diff blob2 blob5)) <*> return s3,
         (~?=) <$> (getDiff (diff blob2 blob6)) <*> return s4,
         (~?=) <$> (getDiff (diff blob2 tree2)) <*> return s5,
         (~?=) <$> (getDiff (diff tree2 tree3)) <*> return s6,
         (~?=) <$> (getDiff (diff tree2 tree4)) <*> return s7,
         (~?=) <$> (getDiff (diff tree5 tree6)) <*> return s8,
         (~?=) <$> (getDiff (diff tree5 tree2)) <*> return s9,
         (~?=) <$> (getDiff (diff tree5 tree4)) <*> return s0]
  _ <- runExceptT (runReaderT (RM.writeObjectToFile tree2 
                                 :: RepoState O.ObjectId) "./test/")
  _ <- runExceptT (runReaderT (RM.writeObjectToFile tree4 
                                 :: RepoState O.ObjectId) "./test/")
  _ <- ((sequence x) >>= (\x -> runTestTT (TestList x)))
  _ <- runTestTT (TestList [importObjectTests, exportObjectTests])
  return ()

s1 = "\nBlob 1:94323\nBlob 2:94323\n\n"
s2 = "\nBlob 1:94323\nBlob 2:47964\n1c1\n< \"LINE1\"\n---\n> \"LINECHANGED2\"\n"
s3 = "\nBlob 1:94323\nBlob 2:94323\n\n"
s4 = "\nBlob 1:94323\nBlob 2:f58f8\n1c1\n< \"LINE1\"\n---\n> \"LINE1\\nLINE3\"\n"
s5 = "\nCannot compare obj 94323,5d88a"
s6 = "\nTree 1:5d88a\nTree 2:9f892\n\n~~New file in Second: alsoblob2\n\"LINE1\"\n~~New file in First: blob2\n\"LINE1\"" 
s7 = "\nTree 1:5d88a\nTree 2:e5395\n\n~~Changes in blob2,blob2\n\nBlob 1:94323\nBlob 2:47964\n1c1\n< \"LINE1\"\n---\n> \"LINECHANGED2\"\n"
s8 = "\nTree 1:50f83\nTree 2:ecf3b\n\n~~Changes in tree2,tree2\n\nTree 1:5d88a\nTree 2:e5395\n\n~~Changes in blob2,blob2\n\nBlob 1:94323\nBlob 2:47964\n1c1\n< \"LINE1\"\n---\n> \"LINECHANGED2\"\n\n~~New folder in Second: treex\n\"name tree2\\nblob 9432366705517808d910ab80041213d86d451913bcb995b0d6d80eb93d66ac59 blob2\\n\"\n~~Changes in blob2,blob2\n\nBlob 1:94323\nBlob 2:94323\n\n\n~~New file in Second: blob3\n\"LINE1\"\n~~Changes in blob4,blob4\n\nBlob 1:94323\nBlob 2:47964\n1c1\n< \"LINE1\"\n---\n> \"LINECHANGED2\"\n\n~~New file in First: blob5\n\"LINE1\""
s9 = "\nTree 1:50f83\nTree 2:5d88a\n\n~~New folder in First: tree2\n\"name tree2\\nblob 9432366705517808d910ab80041213d86d451913bcb995b0d6d80eb93d66ac59 blob2\\n\"\n~~Changes in blob2,blob2\n\nBlob 1:94323\nBlob 2:94323\n\n\n~~New file in First: blob4\n\"LINE1\"\n~~New file in First: blob5\n\"LINE1\""
s0 = "\nTree 1:50f83\nTree 2:e5395\n\n~~New folder in First: tree2\n\"name tree2\\nblob 9432366705517808d910ab80041213d86d451913bcb995b0d6d80eb93d66ac59 blob2\\n\"\n~~Changes in blob2,blob2\n\nBlob 1:94323\nBlob 2:47964\n1c1\n< \"LINE1\"\n---\n> \"LINECHANGED2\"\n\n~~New file in First: blob4\n\"LINE1\"\n~~New file in First: blob5\n\"LINE1\""

getDiff :: RepoState String -> IO String
getDiff x = do
  res <- runExceptT (runReaderT x "./test") 
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
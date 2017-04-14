module Objects (
   Object(..)
  ,ObjectId
  ,ObjectType(..)
  ,toLineTree
  ,toLineBlob
  ,toLineCommit
) where 
import Data.ByteString.Char8 as C

type ObjectId = C.ByteString  
data ObjectType = TCommit | TTree | TBlob deriving (Eq)

instance Show ObjectType where
  show TCommit = "commit"
  show TTree   = "tree"
  show TBlob   = "blob"

data Object = CommitObj Commit | TreeObj Tree | BlobObj Blob

--hashObject (CommitObj c) = hashCommit c

data Commit = Commit {
  tree     :: ObjectId
 ,parents  :: [ObjectId]
 ,author   :: C.ByteString
 ,message  :: C.ByteString
}

data Tree = Tree{
 entries  :: [(String, ObjectId, ObjectType)] -- same object id but different file names
                                                 -- to prevent commit in tree
}

data Blob = Blob{
 content     :: C.ByteString
}

-- pretty printer for commit objects, for example, to write to files
-- put in object?
toLineCommit :: String -> Commit -> C.ByteString
toLineCommit "tree" c   = (C.pack "tree ") `C.append` (tree c) `C.append` (C.pack "\n")
toLineCommit "author" c = (C.pack "author ") `C.append` (author c) `C.append` (C.pack "\n")
toLineCommit "msg" c    = (C.pack "\n") `C.append` (message c) `C.append` (C.pack "\n")
toLineCommit "parent" c = writeParents (parents c) where
  writeParents []     = C.pack ""
  writeParents (p:ps) = (C.pack "parent ") `C.append` p `C.append` (C.pack "\n") `C.append` (writeParents ps)
toLineCommit _ c        = (C.pack "Unexpected property\n")

-- pretty printer for tree objects, for example, to write to files
-- put in object?
toLineTree :: Tree -> C.ByteString 
toLineTree t = writeEntries (entries t) where
  writeEntries []                       = C.pack ""
  writeEntries ((name, id, TBlob):es) = (C.pack "blob ") `C.append` id `C.append` (C.pack (name++"\n")) 
  writeEntries ((name, id, TTree):es) = (C.pack "tree ") `C.append` id `C.append` (C.pack (name++"\n"))
  writeEntries _                        = (C.pack "Unexpected property\n")

-- pretty printer for commit objects, for example, to write to files
-- put in object?
toLineBlob :: Blob -> C.ByteString
toLineBlob = content
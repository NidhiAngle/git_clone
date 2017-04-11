module Objects (
  Commit(..)
  ,Object(..)
  ,ObjectId
  ,ObjectType(..)
  ,Tree(..)
) where 
import Data.ByteString as B

type ObjectId = B.ByteString  
data ObjectType = TCommit | TTree | TBlob deriving (Eq)

instance Show ObjectType where
  show TCommit = "commit"
  show TTree   = "tree"
  show TBlob   = "blob"

data Object = CommitObj Commit | TreeObject Tree | BlobObj Blob

--hashObject (CommitObj c) = hashCommit c

data Commit = Commit {
  tree     :: ObjectId
 ,parents  :: [ObjectId]
 ,author   :: B.ByteString
 ,message  :: B.ByteString
}

data Tree = Tree{
 entries     :: [(String, ObjectId)]
}

data Blob = Blob{
 content     :: B.ByteString
}


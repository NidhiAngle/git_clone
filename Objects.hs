module Objects (
  Commit(..)
  ,Object(..)
  ,ObjectId
  ,ObjectType(..)
  ,Tree(..)
  ,TreeEntry(..)
) where 
import Data.ByteString as B

type ObjectId = String  
data ObjectType = TCommit | TTree | TBlob deriving (Eq)

instance Show ObjectType where
  show TCommit = "commit"
  show TTree   = "tree"
  show TBlob   = "blob"

data Object = Object {
  objType    :: ObjectType
 ,objHash    :: ObjectId
} 
data Commit = Commit {
  tree     :: B.ByteString
 ,parents  :: [B.ByteString]
 ,commitHash     :: B.ByteString
 ,author   :: B.ByteString
 ,message  :: B.ByteString
}

data Tree = Tree{
  treeHash    :: ObjectId
 ,entries     :: [TreeEntry]
}

data TreeEntry = TreeEntry {
  entryHash    :: ObjectId

}


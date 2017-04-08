import Data.ByteString as B
import Crypto.Hash.SHA1 (hash)

type ObjectId = String  
data ObjectType = "commit" | TTree | TBlob deriving (Eq)

instance Show ObjectType where
	show TCommit = "commit"
	show TTree   = "tree"
	show TBlob   = "blob"

data Object = Object {
    objType    :: ObjectType
   ,hash       :: ObjectId	
} 
data Commit = Commit {
	tree     :: B.ByteString
   ,parents  :: [B.ByteString]
   ,hash     :: B.ByteString
   ,author   :: B.ByteString
   ,commiter :: B.ByteString
   ,message  :: B.ByteString
}

data Tree = Tree{
	hash    :: ObjectId
   ,entries :: [TreeEntry]
}

data TreeEntry = TreeEntry {
	hash    :: ObjectId

}
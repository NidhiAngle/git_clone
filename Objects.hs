import Data.ByteString as B
import Crypto.Hash.SHA1 (hash)


data ObjectType = TCommit | TTree | TBlob deriving (Eq)

instance Show ObjectType where
	show TCommit = "Commit"
	show TTree   = "Tree"
	show TBlob   = "Blob"

data Object = Object {
	getBlob :: B.ByteString
   ,type    :: ObjectType
   ,hash    :: 	
} 
data Commit = Commit {
	tree     :: B.ByteString,
	parents  :: [B.ByteString]
	hash     :: B.ByteString
	author   :: String
	commiter :: String
	message  :: String
}
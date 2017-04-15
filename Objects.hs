module Objects (
   Object(..)
  --,Commit(..)-- COMMENT OUT LATER
  --,Tree(..)-- COMMENT OUT LATER
  --,Blob(..)-- COMMENT OUT LATER
  ,ObjectId
  ,toLineTree
  ,toLineBlob
  ,toLineCommit
  ,parseBlob
  ,parseCommit
  ,parseTree
) where 
import Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Char8 as PB
import Control.Applicative ((<|>))
-- import Parser as PB
-- import qualified ParserCombinators as PB
--PARSER- BYTES TO STRING TO BYTES TO STORE 
--remove Blob export

type ObjectId = C.ByteString
type Name = String  
data EntryType = TTree | TBlob deriving (Eq)

instance Show EntryType where
  show TTree   = "tree"
  show TBlob   = "blob"

data Object = CommitObj Commit | TreeObj Tree | BlobObj Blob

--hashObject (CommitObj c) = hashCommit c

data Commit = Commit {
  parents  :: [ObjectId]
 ,tree     :: ObjectId
 ,author   :: C.ByteString
 ,message  :: C.ByteString
}

data Tree = Tree{
 entries  :: [(EntryType, ObjectId, Name)] -- same object id but different file names?
                                                 -- to prevent commit in tree
}

data Blob = Blob{
 content :: C.ByteString
}

bytestr :: String -> PB.Parser ByteString
bytestr s = string $ C.pack s

constP :: String -> a -> PB.Parser a
constP s a = fmap (const a) (bytestr s)

nls :: PB.Parser ByteString
nls = string (C.pack "\n") <|> string (C.pack " ")


parseParent :: PB.Parser C.ByteString
parseParent = do
  pid <- bytestr "parent " *> takeTill (== '\n')
  nls
  pure $ pid

parseEntry :: PB.Parser (EntryType, ObjectId, Name)
parseEntry = do
  etype <- constP "blob " TBlob <|> constP "tree " TTree
  id    <- takeTill (== ' ')
  nls
  name  <- fmap C.unpack $ takeTill (== '\n') 
  nls
  pure $ (etype, id, name)

parseCommit :: PB.Parser Commit 
parseCommit = do
  parents <- many' parseParent
  tree    <- bytestr "tree " *> takeTill (== '\n')
  nls
  author  <- bytestr "author " *> takeTill (== '\n')
  nls
  nls
  msg     <- takeTill (== '\n')
  nls
  pure $ Commit parents tree author msg

parseTree :: PB.Parser Tree
parseTree = do
  entries <- many' parseEntry
  pure $ Tree entries



parseBlob :: PB.Parser Blob
parseBlob = do
  content <- PB.takeByteString
  pure $ Blob content


-- pretty printer for commit objects, for example, to write to files
toLineCommit :: String -> Commit -> C.ByteString
toLineCommit "tree" c   = (C.pack "tree ") `C.append` (tree c) `C.append` (C.pack "\n") -- takeTill solved what about this
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
  writeEntries []                     = C.pack ""
  writeEntries ((TBlob, id , name):es) = (C.pack "blob ") `C.append` id `C.append` (C.pack (name++"\n")) 
  writeEntries ((TTree, id, name):es) = (C.pack "tree ") `C.append` id `C.append` (C.pack (name++"\n"))

-- pretty printer for commit objects, for example, to write to files
-- put in object?
toLineBlob :: Blob -> C.ByteString
toLineBlob = content

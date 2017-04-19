module Objects (
   Object(..)
  ,ObjectId
  ,ObjectName
  ,EntryType
  ,makeCommit
  ,makeTree-- COMMENT OUT LATER
  ,makeBlob-- COMMENT OUT LATER
  ,toLineTree
  ,toLineBlob
  ,toLineCommit
  ,parseBlob
  ,parseCommit
  ,parseTree
  ,bytestr
  ,makeBlobEntryType
  ,makeTreeEntryType

) where 
import Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Char8 as PB
import Control.Applicative ((<|>))
-- import Parser as PB
-- import qualified ParserCombinators as PB
--PARSER- BYTES TO STRING TO BYTES TO STORE 
--remove Blob export

type ObjectId = C.ByteString
type ObjectName = C.ByteString  
data EntryType = TTree | TBlob deriving (Eq)

instance Show EntryType where
  show TTree   = "tree"
  show TBlob   = "blob"

data Object = CommitObj Commit | TreeObj Tree | BlobObj Blob deriving (Eq, Show)

-- YOU CAN JUST DERIVE SHOW 
-- instance Show Object where
--   show (CommitObj c) = show c
--   show (TreeObj t) = show t
--   show (BlobObj b) = show b

-- instance Show Commit where
--   show commit = show (((fmap C.unpack) . parents) commit) ++ show ((C.unpack . tree) commit) ++ show (author commit) ++ show (message commit)

-- instance Show Tree where
--   show tree = show $ Prelude.foldr (\(_,x,_) acc -> acc `C.append` x) C.empty (entries tree)

-- instance Show Blob where
--   show blob = "blob"
--hashObject (CommitObj c) = hashCommit c

data Commit = Commit {
  parents  :: [ObjectId]
 ,tree     :: ObjectId
 ,author   :: C.ByteString
 ,message  :: C.ByteString
} deriving (Eq, Show)

data Tree = Tree{
 entries  :: [(EntryType, ObjectId, ObjectName)] -- same object id but different file names?
                                                 -- to prevent commit in tree
} deriving (Eq, Show)

data Blob = Blob{
 content :: C.ByteString
} deriving (Eq, Show)

makeCommit :: [ObjectId]-> ObjectId -> ByteString -> ByteString -> Object
makeCommit ps n a m= CommitObj $ Commit ps n a m

makeTree :: [(EntryType, ObjectId, ObjectName)] -> Object
makeTree  = TreeObj . Tree

makeBlob :: ByteString -> Object
makeBlob = BlobObj . Blob

makeTreeEntryType = TTree
makeBlobEntryType = TBlob

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
  pure pid

parseEntry :: PB.Parser (EntryType, ObjectId, ObjectName)
parseEntry = do
  etype <- constP "blob " TBlob <|> constP "tree " TTree
  id    <- takeTill (== ' ')
  nls
  name  <- takeTill (== '\n') 
  nls
  pure (etype, id, name)

parseCommit :: PB.Parser Commit 
parseCommit = do
  parents <- many' parseParent
  tree    <- bytestr "tree " *> takeTill (== '\n')
  nls
  author  <- bytestr "author " *> takeTill (== '\n')
  nls
  nls
  msg     <- bytestr "msg " *> takeTill (== '\n')
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
toLineCommit :: Commit -> C.ByteString
toLineCommit c = Prelude.foldl (\b x -> b `C.append` helper "parent " x) 
                 (C.pack "") (parents c) 
                 `C.append` helper "tree " (tree c) 
                 `C.append` helper "author " (author c)
                 `C.append` helper "msg" (message c)
  where
    helper "msg" x    = C.pack "\n" `C.append` x `C.append` C.pack "\n"
    helper str x   = C.pack str `C.append` x `C.append` C.pack "\n"

-- pretty printer for tree objects, for example, to write to files
-- put in object?
toLineTree :: Tree -> C.ByteString 
toLineTree t = Prelude.foldl helper (C.pack "") (entries t)
  where 
    helper base (x, id, name) = case x of
      TBlob -> base `C.append` C.pack "blob " `C.append` id `C.append` 
               C.pack " " `C.append` name `C.append` C.pack "\n"
      TTree -> base `C.append` C.pack "tree " `C.append` id `C.append` 
               C.pack " " `C.append` name `C.append` C.pack "\n"

-- pretty printer for commit objects, for example, to write to files
toLineBlob :: Blob -> C.ByteString
toLineBlob = content
-------------------------------------


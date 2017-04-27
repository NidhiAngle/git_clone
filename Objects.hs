
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Objects (
   Object(..)
  ,ObjectId
  ,ObjectName
  ,EntryType(TTree, TBlob)
  ,TreeEntry
  ,Commit(parents, dateTime, tree)
  ,Blob(content)
  ,Tree(name, entries)
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
  ,getTreeFromCommit
  ,getEntries
  ,getTreeName
  ,getAuthor
  ,getTree
  ,getParents
) where 
import qualified Data.ByteString.Char8 as C
import qualified Data.Attoparsec.ByteString.Char8 as PB
import Control.Applicative ((<|>))
import qualified Data.Time.Clock as DT
import qualified Data.Time.Format as DTF

-- | Definition of all the objects including blob, 
-- | commit, tree their constructors, pretty printer
-- | and parser

type ObjectId = C.ByteString
type ObjectName = C.ByteString  
data EntryType = TTree | TBlob deriving (Eq)

instance Show EntryType where
  show TTree   = "tree"
  show TBlob   = "blob"

instance Ord EntryType where
  compare TTree TBlob = LT
  compare TBlob TTree = GT
  compare _ _         = EQ

data Object = CommitObj Commit | TreeObj Tree | BlobObj Blob deriving (Eq, Show)

data Commit = Commit {
  parents  :: [ObjectId]
 ,tree     :: ObjectId
 ,author   :: C.ByteString
 ,message  :: C.ByteString
 ,dateTime :: DT.UTCTime
} deriving (Show)

instance Eq Commit where
 (==) c1 c2 = (==) (dateTime c1) (dateTime c2)

instance Ord Commit where
  compare c1 c2 = compare (dateTime c2) (dateTime c1)

type TreeEntry = (EntryType, ObjectId, ObjectName)

data Tree = Tree{
 name :: C.ByteString,
 entries  :: [TreeEntry] -- same object id but different file names?
                                                 -- to prevent commit in tree
} deriving (Eq,Show)


data Blob = Blob{
 content :: C.ByteString
} deriving (Eq, Show)

-- | Useful getters
getAuthor :: Commit -> C.ByteString
getAuthor = author

getTree :: Commit -> ObjectId
getTree = tree

getParents :: Commit -> [ObjectId]
getParents = parents

getTreeFromCommit :: Commit -> ObjectId
getTreeFromCommit = tree 

getEntries :: Tree -> [TreeEntry]
getEntries = entries

getTreeName :: Tree -> C.ByteString
getTreeName = name

-- | Constructors visible to the outside

makeCommit :: [ObjectId]-> ObjectId -> C.ByteString -> C.ByteString -> 
              DT.UTCTime -> Object
makeCommit ps n a m t= CommitObj $ Commit ps n a m t

makeTree :: C.ByteString -> [TreeEntry] -> Object
makeTree n es = TreeObj $ Tree n es

makeBlob :: C.ByteString -> Object
makeBlob = BlobObj . Blob

makeTreeEntryType = TTree
makeBlobEntryType = TBlob

-- | Parsers and helper functions for the commit, 
-- | tree and blob

bytestr :: String -> PB.Parser C.ByteString
bytestr s = PB.string $ C.pack s

constP :: String -> a -> PB.Parser a
constP s a = fmap (const a) (bytestr s)

nls :: PB.Parser C.ByteString
nls = PB.string (C.pack "\n") <|> PB.string (C.pack " ")

parseParent :: PB.Parser C.ByteString
parseParent = do
  pid <- bytestr "parent " *> PB.takeTill (== '\n')
  nls
  pure pid

parseEntry :: PB.Parser (EntryType, ObjectId, ObjectName)
parseEntry = do
  etype <- constP "blob " TBlob <|> constP "tree " TTree
  id    <- PB.takeTill (== ' ')
  nls
  name  <- PB.takeTill (== '\n') 
  nls
  pure (etype, id, name)

parseCommit :: PB.Parser Commit 
parseCommit = do
  parents <- PB.many' parseParent
  tree    <- bytestr "tree " *> PB.takeTill (== '\n')
  nls
  author  <- bytestr "author " *> PB.takeTill (== '\n')
  nls
  nls
  msg     <- PB.takeTill (== '\n')
  nls
  time    <- bytestr "time " *> PB.takeTill (== '\n')
  nls
  let dateTime = 
          DTF.parseTimeOrError True DTF.defaultTimeLocale "%s" (C.unpack time)
  pure $ Commit parents tree author msg dateTime

parseTree :: PB.Parser Tree
parseTree = do
  name <- bytestr "name " *> PB.takeTill (== '\n')
  nls
  entries <- PB.many' parseEntry
  pure $ Tree name entries



parseBlob :: PB.Parser Blob
parseBlob = do
  content <- PB.takeByteString
  pure $ Blob content


-- | pretty printer for commit trees and blobs, for example, to write to files

toLineCommit :: Commit -> C.ByteString
toLineCommit c = Prelude.foldl (\b x -> b `C.append` helper "parent " x) 
                 (C.pack "") (parents c) 
                 `C.append` helper "tree " (tree c) 
                 `C.append` helper "author " (author c)
                 `C.append` helper "msg" (message c)
                 `C.append` helper "time " (C.pack (DTF.formatTime 
                                                      DTF.defaultTimeLocale 
                                                      "%s" (dateTime c)))
  where
    helper "msg" x    = C.pack "\n" `C.append` x `C.append` C.pack "\n"
    helper str x   = C.pack str `C.append` x `C.append` C.pack "\n"

toLineTree :: Tree -> C.ByteString 
toLineTree t = Prelude.foldl helper 
                (C.pack "name " `C.append` name t `C.append` C.pack "\n") 
                (entries t)
  where 
    helper base (x, id, name) = case x of
      TBlob -> base `C.append` C.pack "blob " `C.append` id `C.append` 
               C.pack " " `C.append` name `C.append` C.pack "\n"
      TTree -> base `C.append` C.pack "tree " `C.append` id `C.append` 
               C.pack " " `C.append` name `C.append` C.pack "\n"


toLineBlob :: Blob -> C.ByteString
toLineBlob = content
-------------------------------------


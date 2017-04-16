{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}
{-# LANGUAGE PackageImports        #-}

module ObjectStore(
  Repo
 ,getObjPath
 ,exportObject
 ,importObject
 ,readObject
  ) where
--import "cryptohash" Crypto.Hash
import Data.ByteString.Char8 as C
import qualified Objects as O
import Text.Printf (printf)
import System.FilePath
import Control.Monad ()
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 as PB



type Repo = String

-- Given the name of the repository and id, this gives you the filepath
getObjPath :: Repo -> O.ObjectId -> FilePath
getObjPath r o = (r </> ".git" </> "objects"  </> (Prelude.take 2 (C.unpack o)) </> (Prelude.drop 2 (C.unpack o)))

hexSha256 :: ByteString -> ByteString
hexSha256 b = C.pack "abcdefghijklmnopqrstuvwxyz12345678901234"
--hexSha256 bs = digestToHexByteString ((hash bs) :: Digest SHA256)

-- Given object, adds header and hashes to give
-- id and new content
hashContent :: O.Object-> (O.ObjectId, C.ByteString)
hashContent obj = do
  let content          = objToByte obj
      header           = getHeader obj ((show . C.length) content)
      headerAndContent = header `C.append` content
      id               = hexSha256 headerAndContent
  (id, headerAndContent) 
  where 
      getHeader (O.CommitObj c) l = (C.pack "commit ") `C.append`
                                    (C.pack (l++"\0"))
      getHeader (O.TreeObj c) l   = (C.pack "tree ") `C.append`
                                    (C.pack (l++"\0"))
      getHeader (O.BlobObj c) l   = (C.pack "blob ") `C.append`
                                    (C.pack (l++"\0"))

-- Consolidates a object type to a bytestring
objToByte :: O.Object -> C.ByteString
objToByte (O.CommitObj c) = O.toLineCommit c
objToByte (O.TreeObj t)   = O.toLineTree t
objToByte (O.BlobObj b)   = O.toLineBlob b

exportObject :: Monad m => Repo -> O.Object -> (m FilePath,m FilePath, m C.ByteString)
exportObject r obj= do
  let (id, content) = hashContent obj
      path  = getObjPath r id
  (return (takeDirectory path), return path, return content)

parseHeader :: String -> Parser ByteString
parseHeader str = O.bytestr str *> takeTill (=='\0')
-- DONT FORGET TO ADD THIS
parseObject :: Parser O.Object
parseObject = fmap O.CommitObj O.parseCommit <|> 
              fmap O.TreeObj O.parseTree <|> 
              fmap O.BlobObj O.parseBlob

readObject :: ByteString -> Maybe O.Object
readObject str = case parseOnly parseObject str of
  Right obj -> Just obj
  _         -> Nothing

importObject :: Monad m => m ByteString -> m (Maybe O.Object)
importObject = fmap readObject 
------------------------------------------------


-------------------------------------------------------------------






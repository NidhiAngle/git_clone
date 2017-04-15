{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}
{-# LANGUAGE PackageImports        #-}

module ObjectStore where
--import "cryptonite" Crypto.Hash
import Data.ByteString.Char8 as C
import qualified Objects as O
import Text.Printf (printf)
import System.FilePath
import Control.Monad ()
import Control.Monad.IO.Class
import System.Directory
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Lazy as B


type Repo = String

-- Given the name of the repository and id, this gives you the filepath
getObjPath :: Repo -> O.ObjectId -> FilePath
getObjPath r o = (r </> ".git" </> "objects"  </> (Prelude.take 2 (C.unpack o)) </> (Prelude.drop 2 (C.unpack o)))

hexSha3_512 :: ByteString -> C.ByteString
hexSha3_512 bs = C.pack "89abcdefghijklmnopqlksnghmamnfajehrkajkg"
--hexSha3_512 bs = show (hash bs :: Digest SHA3_512)

-- Given object, adds header and hashes to give
-- id and new content
hashContent :: O.Object-> (O.ObjectId, C.ByteString)
hashContent obj = do
  let content          = objToByte obj
      header           = getHeader obj ((show . C.length) content)
      headerAndContent = header `C.append` content
      id               = hexSha3_512 headerAndContent
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
objToByte (O.CommitObj c) = (O.toLineCommit "parent" c) `C.append` (O.toLineCommit "tree" c) `C.append` 
                            (O.toLineCommit "author" c) `C.append` (O.toLineCommit "msg" c)
objToByte (O.TreeObj t)   = O.toLineTree t
objToByte (O.BlobObj b)   = O.toLineBlob b

exportObject :: Monad m => Repo -> O.Object -> (m FilePath,m FilePath, m C.ByteString)
exportObject r obj= do
  let (id, content) = hashContent obj
      path  = getObjPath r id
  (return (takeDirectory path), return path, return content)

-- importObject :: Monad m => m C.ByteString -> m FilePath -> O.Object
-- importObject

-------------------------------------------------------------------

writeObjectToFile :: Repo -> O.Object -> IO ()
writeObjectToFile r o = do
    let (path, name, content) = exportObject r o
    _ <- path >>= (createDirectoryIfMissing True)  
    (name >>= (return . B.writeFile)) <*> (compress content) 
    return () 
    where
        compress :: IO C.ByteString -> IO B.ByteString
        compress mx =  mx >>= (\x -> return ((Zlib.compress . B.fromChunks) [x]))

writeObject :: Repo -> O.Object -> IO String
writeObject r o = do
    let (path, name, content) = exportObject r o
    content >>= (return . show) 
 -- Why cant I use (putStr . show) even after changing to IO ()
    
hi = Prelude.putStrLn "hi"


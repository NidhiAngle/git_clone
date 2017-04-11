{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}
{-# LANGUAGE PackageImports        #-}

module ObjectStore where
--import "cryptohash" Crypto.Hash
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

getObjPath :: Repo -> O.ObjectId -> FilePath
getObjPath r o = (r </> ".git" </> "objects"  </> (Prelude.take 2 o) </> (Prelude.drop 2 o))


hashContent :: O.ObjectType -> C.ByteString -> (O.ObjectId, C.ByteString)
hashContent objType content = do
  let header           = getHeader (C.pack (show objType)) 
      headerAndContent = header `C.append` content
      id               = hexSha3_512 headerAndContent
  (id, headerAndContent) 
  where 
      getHeader objType  = objType `C.append` (C.pack " ") `C.append` (C.pack (show (C.length content))) `C.append` (C.pack "\0")

exportObject :: MonadIO m => Repo -> C.ByteString -> O.ObjectType -> (m FilePath,m FilePath, m C.ByteString)
exportObject r content objtype = do
  let (id, content) = hashContent objtype content
      path  = getObjPath r id
  (return (takeDirectory path), return path, return content)

hexSha3_512 :: ByteString -> String
hexSha3_512 bs = "89abcdefghijklmnopqlksnghmamnfajehrkajkg"
--hexSha3_512 bs = show (hash bs :: Digest SHA3_512)
-------------------------------------------------------------------

--writeObject :: Repo -> C.ByteString -> O.ObjectType -> IO ()
writeObject r c t = do
    let (path, name, content) = exportObject r c t
    _ <- path >>= (createDirectoryIfMissing True)  
    (name >>= (return . B.writeFile)) <*> (compress content) 
    return () 
    where
        compress :: IO C.ByteString -> IO B.ByteString
        compress mx =  mx >>= (\x -> return ((Zlib.compress . B.fromChunks) [x]))
    
hi = Prelude.putStrLn "hi"



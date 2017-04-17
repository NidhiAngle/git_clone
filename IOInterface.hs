import qualified Objects as O
import System.Directory
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Lazy as B
import qualified ObjectStore as OS
import Data.ByteString.Char8 as C

writeObjectToFile :: OS.Repo -> O.Object -> IO ()
writeObjectToFile r o = do
    let (path, name, content) = OS.exportObject r o
    _ <- path >>= (createDirectoryIfMissing True)  
    (name >>= (return . B.writeFile)) <*> (compress content) 
    return () 
    where
        compress :: IO C.ByteString -> IO B.ByteString
        compress mx =  mx >>= (\x -> return ((Zlib.compress . B.fromChunks) [x]))

writeObject :: OS.Repo -> O.Object -> IO String
writeObject r o = do
    let (path, name, content) = OS.exportObject r o
    content >>= (return . show) 
 -- Why cant I use (putStr . show) even after changing to IO ()
    
readObjectFromFile :: OS.Repo -> O.ObjectId -> IO (Maybe O.Object)
readObjectFromFile r id = do 
  let filename = OS.getObjPath r id 
  exists <- doesFileExist filename
  if exists then do
    bs <- C.readFile filename
    return $ OS.readObject $ inflate bs
  else return Nothing
  where inflate blob = C.concat . B.toChunks . Zlib.decompress $ B.fromChunks [blob] 

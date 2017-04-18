import qualified Objects as O
import System.Directory
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Lazy as B
import qualified ObjectStore as OS
import Data.ByteString.Char8 as C

writeObjectToFile :: OS.Repo -> O.Object -> IO ()
writeObjectToFile r o = do
    let (pathIO, nameIO, contentIO) = OS.exportObject r o
    path <- pathIO
    name <- nameIO
    content <- compress contentIO
    createDirectoryIfMissing True path  
    B.writeFile name content
    return () 
    where
        compress :: IO C.ByteString -> IO B.ByteString
        compress mxs = do mx <- mxs
                          return ((Zlib.compress . B.fromChunks) [mx])

writeObject :: OS.Repo -> O.Object -> IO String
writeObject r o = do
    let (path, name, content) = OS.exportObject r o
    fmap show content 
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

testRead :: OS.Repo -> FilePath -> IO ()
testRead r f = do
  maybeObj <- readObjectFromFile r (C.pack f)
  case maybeObj of
    Just obj -> Prelude.putStrLn $ C.unpack (OS.objToByte obj) 
    Nothing -> Prelude.putStrLn "Found nothing"

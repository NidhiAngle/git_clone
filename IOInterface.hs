import qualified Objects as O
import System.Directory
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Lazy as B
import qualified ObjectStore as OS
import Data.ByteString.Char8 as C
import Control.Monad 

writeObjectToFile :: OS.Repo -> O.Object -> IO (String)
writeObjectToFile r o = do
    let (pathIO, nameIO, contentIO) = OS.exportObject r o
    path <- pathIO
    name <- nameIO
    content <- compress contentIO
    createDirectoryIfMissing True path  
    B.writeFile name content
    return name 
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
    Just obj -> (writeObject r obj) >>= Prelude.putStrLn 
    Nothing  -> Prelude.putStrLn "Found nothing"

--ASSUMPTION NO REMOTES
--readRefs OS.Repo
readRefs repo ref = do
  branches <- listDirectory (repo ++ "/.git/refs/heads")
  addRefs branches
  where
    addRefs []     = return ref
    addRefs (b:bs) = do
                       id <- C.readFile b
                       return $ OS.addRef ref (C.pack b) id    

createEmptyRepo :: OS.Repo -> IO ()
createEmptyRepo repo = Prelude.mapM_ (createDirectoryIfMissing True) folders
    where folders = [repo ++ "/.git/objects", repo ++ "/.git/refs/heads"]

initialize :: OS.Repo -> OS.Ref -> IO(OS.Ref)
initialize r ref = do
  dexists <- doesDirectoryExist (r ++ "/.git") 
  if dexists then do
    Prelude.putStrLn "Reinitializing git directory"
    readRefs r ref
  else do
    createEmptyRepo r
    Prelude.putStrLn "Initializing git directory"
    return ref
  
userInterface :: IO()
userInterface = go OS.createRef where
  go refMap = do
    Prelude.putStr "git> "
    str <- Prelude.getLine
    case str of
      "init" -> initialize "nidhiTest" refMap >>= go 
      "exit" -> do 
                 Prelude.putStrLn $ show refMap
                 return ()



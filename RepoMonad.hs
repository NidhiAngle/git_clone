{-# LANGUAGE FlexibleInstances #-}
-- ask about this versus newtype

module RepoMonad where
import qualified Objects as O
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Lazy as B
import qualified ObjectStore as OS
import qualified Data.ByteString.Char8 as C
import Control.Monad 
import Control.Monad.Except
import System.Directory (createDirectoryIfMissing, listDirectory,
                         doesDirectoryExist,doesFileExist)
import System.FilePath (splitFileName)



class RepoMonad m where
   readObjectFromFile :: OS.Repo -> O.ObjectId -> m O.Object
   writeObjectToFile :: OS.Repo -> O.Object -> m O.ObjectId
   writeObject :: OS.Repo -> O.Object -> m String

instance RepoMonad (ExceptT String IO) where

  readObjectFromFile r id = do 
    let filename = OS.getObjPath r id 
    exists <- liftIO $ doesFileExist filename
    if exists then do
      bs <- liftIO $ C.readFile filename
      case OS.readObject (inflate bs) of
        Just x  -> return x
        Nothing -> throwError $ "This object is not valid"
    else throwError $ "This file does not exist"
    where inflate blob = C.concat . 
                         B.toChunks . 
                         Zlib.decompress $ 
                         B.fromChunks [blob] 

  writeObjectToFile r o = do
    let (path, name, content) = OS.exportObject r o
    liftIO $ createDirectoryIfMissing True path  
    liftIO $ B.writeFile (OS.getObjPath r name) (compress content)

    return name
    where
        compress :: C.ByteString -> B.ByteString
        compress mx = (Zlib.compress . B.fromChunks) [mx]

  writeObject r o = do
    let (path, name, content) = OS.exportObject r o
    return $ C.unpack content
 -- Why cant I use (putStr . show) even after changing to IO ()

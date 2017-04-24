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
import Data.Monoid
import System.Directory (createDirectoryIfMissing, listDirectory,
                         doesDirectoryExist,doesFileExist)
import System.FilePath (splitFileName)
import Data.Functor.Classes

class RepoMonad m where
   readObjectFromFile :: OS.Repo -> O.ObjectId -> m O.Object
   writeObjectToFile :: OS.Repo -> O.Object -> m O.ObjectId
   writeObject :: OS.Repo -> O.Object -> m String
   getHeadRef :: OS.Repo -> m OS.Ref
   setHead :: OS.Repo -> OS.Branch -> OS.Ref -> m ()
   readRefs :: OS.Repo -> OS.RefStore -> m OS.RefStore
   repomappend :: (Monoid a) => m a -> m a -> m a

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


  
-- First one else where??
  setHead r b ref = do
    liftIO $ C.writeFile (r ++ "/.hit/refs/heads/" ++ b) ref 
    liftIO $ C.writeFile (r ++ "/.hit/" ++ "HEAD") (C.pack ("refs/heads/" ++ b))

  getHeadRef r = do
    isFile <- liftIO $ doesFileExist $ r ++ ".hit/HEAD" 
    if isFile then do
    branch <- liftIO $ C.readFile (r ++ ".hit/HEAD")
    let branchPath = r ++ ".hit/" ++ C.unpack branch
    isFile' <- liftIO $ doesFileExist branchPath 
    if isFile' 
    then do
      branchName <- liftIO $ C.readFile branchPath 
      return branchName
    else return (C.pack "")
    else throwError $ "HEAD file does not exist"

   
  
  readRefs repo ref = do
    exists   <- liftIO $ doesDirectoryExist (repo ++ "/.hit/refs/heads")
    if exists then do
      branches <- liftIO $ listDirectory (repo ++ "/.hit/refs/heads")
      addRefs branches 
    else throwError $ "refs/heads does not exist, no new refs added to store"
    where
        addRefs []     = return ref
        addRefs (b:bs) = do
                           id <- liftIO $ C.readFile (repo ++ "/.hit/refs/heads/" ++ b)
                           return $ OS.addRef ref (C.pack b) id
  
  repomappend x y = mappend <$> x <*> y 

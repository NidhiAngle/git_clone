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
import Control.Monad.Trans.Reader

class (Monad m) => RepoMonad m where
   readObjectFromFile :: O.ObjectId -> m O.Object
   writeObjectToFile :: O.Object -> m O.ObjectId
   writeObject :: O.Object -> m String
   getHeadRef :: m OS.Ref
   addBranch :: FilePath -> m OS.Ref
   getRepo :: m OS.Repo
   setHead :: OS.Branch -> OS.Ref -> m ()
   readRefs :: OS.RefStore -> m OS.RefStore
   repomappend :: (Monoid a) => m a -> m a -> m a

type RepoState = ReaderT OS.Repo (ExceptT String IO)

instance RepoMonad (RepoState) where
  readObjectFromFile id = do
    r <- ask 
    let filename = OS.getObjPath r id 
    exists <- liftIO $ doesFileExist filename
    if exists then do
      bs <- liftIO $ C.readFile filename
      case OS.readObject (inflate bs) of
        Just x  -> return x
        Nothing -> throwError $ "This object is not valid"
    else throwError $ "This file does not exist: " ++ filename
    where inflate blob = C.concat . 
                         B.toChunks . 
                         Zlib.decompress $ 
                         B.fromChunks [blob] 

  writeObjectToFile o = do
    r <- ask
    let (path, name, content) = OS.exportObject r o
    liftIO $ createDirectoryIfMissing True path  
    liftIO $ B.writeFile (OS.getObjPath r name) (compress content)
    return name
    where
        compress :: C.ByteString -> B.ByteString
        compress mx = (Zlib.compress . B.fromChunks) [mx]

  writeObject o = do
    r <- ask
    let (path, name, content) = OS.exportObject r o
    return $ C.unpack content

  getRepo = ask

  
-- First one else where??
  setHead b ref = do
    r <- ask
    liftIO $ C.writeFile (r ++ "/.hit/refs/heads/" ++ b) ref 
    liftIO $ C.writeFile (r ++ "/.hit/" ++ "HEAD") (C.pack ("refs/heads/" ++ b))

  addBranch b = do
  	r <- ask
  	hr <- getHeadRef
  	liftIO $ C.writeFile (r ++ "/.hit/refs/heads/" ++ b) hr
  	return hr

  getHeadRef = do
    r <- ask
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

  
  readRefs ref = do
    r <- getRepo
    exists <- liftIO $ doesDirectoryExist (r ++ "/.hit/refs/heads")
    if exists then do
      branches <- liftIO $ listDirectory (r ++ "/.hit/refs/heads")
      addRefs branches ref
    else throwError $ "refs/heads does not exist, no new refs added to store"
    where
        addRefs [] ref     = return ref
        addRefs (b:bs) ref = do
                           r <- getRepo
                           id <- liftIO $ C.readFile (r ++ "/.hit/refs/heads/" ++ b)
                           addRefs bs (OS.addRef ref (C.pack b) id)
  
  repomappend x y = mappend <$> x <*> y 

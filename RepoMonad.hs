{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
                         doesDirectoryExist,doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath (takeDirectory, splitFileName)
import Data.Functor.Classes
import Control.Monad.Trans.Reader
import qualified Data.Time.Clock as DT
import Data.List (isSuffixOf)

type Author = C.ByteString
type Message = C.ByteString

class (Monad m) => RepoMonad m where
   readObjectFromFile :: O.ObjectId -> m O.Object
   writeObjectToFile :: O.Object -> m O.ObjectId
   writeObject :: O.Object -> m O.ObjectId
   getHeadRef :: m OS.Ref
   addBranch :: FilePath -> m OS.Ref
   getRepo :: m OS.Repo
   setHead :: OS.Branch -> OS.Ref -> m ()
   readRefs :: OS.RefStore -> m OS.RefStore
   workingDirectoryId :: (O.Object -> m O.ObjectId) -> m O.ObjectId
   isWorkingDirectoryDirty :: m Bool
   switchToBranch :: OS.Branch -> m ()
   commit :: (O.Object -> m O.ObjectId) -> [OS.Ref] -> Author -> Message -> m O.ObjectId
   extractTreeToDisk :: O.ObjectId -> m ()
   repomappend :: (Monoid a) => m a -> m a -> m a

type RepoState = ReaderT OS.Repo (ExceptT String IO)

instance RepoMonad (RepoState) where
  readObjectFromFile id = do
    r <- getRepo
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
    r <- getRepo
    let (path, name, content) = OS.exportObject r o
    liftIO $ createDirectoryIfMissing True path  
    liftIO $ B.writeFile (OS.getObjPath r name) (compress content)
    return name
    where
        compress :: C.ByteString -> B.ByteString
        compress mx = (Zlib.compress . B.fromChunks) [mx]

  writeObject o = do
    r <- getRepo
    let (path, name, content) = OS.exportObject r o
    return name

  isWorkingDirectoryDirty = do
    hr <- getHeadRef
    o <- readObjectFromFile hr 
    case o of
        (O.CommitObj c) -> do
                         cwd <- workingDirectoryId writeObject
                         return $ O.tree c == cwd
        _ -> throwError "Expecting a commit object type"  

  workingDirectoryId f = do
    r <- getRepo
    treeEntries <- traverseDirectories f r
    let tree = O.makeTree (C.pack r) treeEntries
    f tree where
      traverseDirectories f fp = do
          filePaths <- liftIO $ fmap (fmap ((++) (fp++"/"))) $ listDirectory fp
          foldr examine (return []) filePaths where
          examine dir = liftM2 mappend (examineEachDirectory f dir) 
          examineEachDirectory f filepath = do
              entry <- examineDirectory f filepath
              case entry of
                Just x  -> return [x]
                Nothing -> return []
      examineDirectory f filePath = do
        isDirectory <- liftIO $ doesDirectoryExist filePath
        isFile <- liftIO $ doesFileExist filePath 
        case (isDirectory, isFile) of  
          (True, _) -> do 
              treeEntries <- traverseDirectories f filePath
              let tree = O.makeTree (C.pack filePath) treeEntries 
              filename <- f tree
              return $ Just (O.makeTreeEntryType, filename, C.pack filePath)
          (_, True) -> do
               contents <- liftIO $ C.readFile filePath
               let blob = O.makeBlob contents
               fileName <- f blob
               return $ Just (O.makeBlobEntryType, fileName, C.pack filePath)
          (_,_)  ->  return Nothing 

  commit f refs a m = do
    filename <- workingDirectoryId f
    utc <- liftIO DT.getCurrentTime
    let c = O.makeCommit refs filename a m utc
    f c 

  switchToBranch branch = do
    r <- getRepo
    ref <- liftIO $ C.readFile (r ++ "/.hit/refs/heads/" ++ branch) 
    setHead branch ref
    filePaths <- liftIO $ listDirectory r
    liftIO $ foldr deleteFp (return ()) filePaths
    ro <- readObjectFromFile ref
    case ro of 
          (O.CommitObj c) -> extractTreeToDisk (O.tree c)  
          (O.TreeObj t) -> throwError $ "Read tree object " ++ show t ++ " instead of commit"
          (O.BlobObj b) -> throwError $ "Read blob object " ++ show b ++ " instead of commit"

    where
      deleteFp :: FilePath -> IO () -> IO ()
      deleteFp fp acc = do
                        let dir = takeDirectory fp
                        if any (isSuffixOf  dir) [".hit",".git"] then
                          acc
                        else do
                        isDirectory <- liftIO $ doesDirectoryExist fp 
                        if isDirectory then 
                           removeDirectoryRecursive fp >> acc
                        else removeFile fp >> acc


  extractTreeToDisk treeId  = do
    ro <- readObjectFromFile treeId
    case ro of
         (O.TreeObj t) -> do
                          liftIO $ createDirectoryIfMissing True ((C.unpack . O.name) t)
                          handleEntries (O.entries t) 
         (O.CommitObj c) -> throwError "Expecting tree, but received commit"
         (O.BlobObj b) -> throwError "Expecting tree, but received blob"

    where
      handleEntries [] = return ()
      handleEntries (e@(t, oId, oName):es) = 
        case t of
          O.TTree -> return ()
          O.TBlob -> do
             ro <- readObjectFromFile oId
             case ro of
                   (O.BlobObj b') -> do
                                     liftIO $ C.writeFile (C.unpack oName) (O.content b') 
                                     handleEntries es
                   _              -> throwError "Found another object instead of blob"


  getRepo = ask

  setHead b ref = do
    r <- getRepo
    liftIO $ C.writeFile (r ++ "/.hit/refs/heads/" ++ b) ref 
    liftIO $ C.writeFile (r ++ "/.hit/" ++ "HEAD") (C.pack ("refs/heads/" ++ b))

  addBranch b = do
        r <- getRepo
        hr <- getHeadRef
        liftIO $ C.writeFile (r ++ "/.hit/refs/heads/" ++ b) hr
        return hr

  getHeadRef = do
    r <- getRepo
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






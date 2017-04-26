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
import qualified Data.Time.Clock as DT

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
   switchToBranch :: O.ObjectId -> m (IO ())
   commit :: (O.Object -> m O.ObjectId) -> [OS.Ref] -> Author -> Message -> m O.ObjectId
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
    let o = OS.readObject hr
    case o of
      Just o' -> case o' of
        (O.CommitObj c) -> do
                         cwd <- workingDirectoryId writeObject
                         return $ O.tree c == cwd
        _ -> throwError "Expecting a commit object type"  
      Nothing -> throwError "Unable to find HEAD ref"

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

  switchToBranch ref = do
    r <- getRepo
    return $ putStrLn "yay!"




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






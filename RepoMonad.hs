{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- ask about this versus newtype

module RepoMonad where

import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Lazy as B
import qualified ObjectStore as OS
import qualified Data.ByteString.Char8 as C
import Control.Monad 
import Control.Monad.Except
import Data.Monoid
import System.FilePath (takeDirectory, splitFileName)
import Data.Functor.Classes
import Control.Monad.Trans.Reader
import qualified Objects as O
import qualified Data.Time.Clock as DT
import Data.List (isSuffixOf, isPrefixOf)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified GitRevisions as GR
import System.Directory (
        createDirectoryIfMissing, 
        listDirectory,
        doesDirectoryExist,
        doesFileExist, 
        removeDirectoryRecursive, 
        removeFile)

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
   updateBranchRef :: OS.Branch -> O.ObjectId -> m ()
   readRefs :: OS.RefStore -> m OS.RefStore
   workingDirectoryId :: (O.Object -> m O.ObjectId) -> m O.ObjectId
   isWorkingDirectoryDirty :: m Bool
   switchToBranch :: OS.Branch -> m ()
   commit :: (O.Object -> m O.ObjectId) -> [OS.Ref] -> 
             Author -> Message -> m O.ObjectId
   extractTreeToDisk :: O.ObjectId -> m ()
   repomappend :: (Monoid a) => m a -> m a -> m a
   getLog :: m (IO ()) 
   initialize :: OS.RefStore -> m OS.RefStore

type RepoState = ReaderT OS.Repo (ExceptT String IO) 

instance RepoMonad RepoState where
  readObjectFromFile id = do
    r <- getRepo
    let filename = OS.getObjPath r id 
    exists <- liftIO $ doesFileExist filename
    if exists then do
      bs <- liftIO $ C.readFile filename
      case OS.readObject (inflate bs) of
        Just x  -> return x
        Nothing -> throwError "This object is not valid"
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

  getLog = do 
    headRef <- getHeadRef
    cId <- liftIO $ C.readFile (C.unpack headRef)
    headCommit <- readObjectFromFile cId
    case headCommit of
      (O.CommitObj c) -> 
        do
        commitLog <- GR.revParseTree [GR.RevId c] getCommitParent 
        return $ printLogs commitLog 
      _               -> 
        return $ putStrLn $ "Not a commit object" ++ C.unpack headRef
    where
    printLogs logSet = mapM_ sepCommit (Set.toList logSet)
    sepCommit x = putStrLn (C.unpack (O.toLineCommit x) ++ "\n" ++ 
                            replicate 80 '~') 

  initialize ref = do
      r <- getRepo
      dexists <- liftIO $ doesDirectoryExist (r ++ "/.hit") 
      if dexists then 
        readRefs ref
      else do
        createEmptyRepo             
        return ref

  isWorkingDirectoryDirty = do
    hr  <- getHeadRef
    cId <- liftIO $ C.readFile (C.unpack hr)
    o   <- readObjectFromFile cId 
    case o of
        (O.CommitObj c) -> do
                         cwd <- workingDirectoryId writeObject
                         return $ O.tree c /= cwd
        _ -> throwError "Expecting a commit object type"  

  workingDirectoryId f = do
    r <- getRepo
    treeEntries <- traverseDirectories f r
    let tree = O.makeTree (C.pack r) treeEntries
    f tree where
      traverseDirectories f fp = do
          filePaths <- liftIO $ fmap ((fp++"/")++) <$> listDirectory fp
          foldr examine (return []) filePaths where
          examine dir = liftM2 mappend (examineEachDirectory f dir) 
          examineEachDirectory f filepath = do
              entry <- examineDirectory f filepath
              case entry of
                Just x  -> return [x]
                Nothing -> return []
      examineDirectory f filePath = 
        if foldr (\test bool -> isSuffixOf test filePath || bool) 
                  False ignorePaths then
           return Nothing
        else do
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
      ignorePaths = [".hit",".git", ".gitignore", ".DS_Store"] 

  commit f refs a m = do
    filename <- workingDirectoryId f
    utc      <- liftIO DT.getCurrentTime
    cIds     <- liftIO $ getCommitIds refs
    let c     = O.makeCommit (Set.toList (Set.fromList cIds)) filename a m utc
    f c 
    where
      getCommitIds refs = 
       filter (\x -> 0 < C.length x) <$> sequence (fmap tryToRead refs)
      tryToRead ref = do
        let ref' = C.unpack ref 
        fileExists <- doesFileExist ref'
        if fileExists then
          C.readFile ref'
        else 
          return $ C.pack ""


  switchToBranch branch = do
    r <- getRepo
    let branchPath = branchBuilder r branch
    ref <- liftIO $ C.readFile branchPath
    setHead branch ref
    filePaths <- liftIO $ fmap (fmap (r ++)) (listDirectory r)
    liftIO (foldr deleteFp (return ()) filePaths >> putStrLn (C.unpack ref))
    ro <- readObjectFromFile ref
    case ro of 
          (O.CommitObj c) -> extractTreeToDisk (O.tree c)  
          (O.TreeObj t) -> throwError $ "Read tree object " ++ show t ++ 
                                        " instead of commit"
          (O.BlobObj b) -> throwError $ "Read blob object " ++ show b ++ 
                                        " instead of commit"

    where
      deleteFp :: FilePath -> IO () -> IO ()
      deleteFp fp acc = 
        if foldr (isInvalidPath fp) False [".hit",".git", ".gitignore"] then
        acc
        else do
        isDirectory <- liftIO $ doesDirectoryExist fp 
        if isDirectory then 
           removeDirectoryRecursive fp >> acc
        else removeFile fp >> acc
      isInvalidPath fp test bool = isSuffixOf test fp || bool


  extractTreeToDisk treeId  = do
    ro <- readObjectFromFile treeId
    case ro of
       (O.TreeObj t) -> 
         do
         liftIO $ createDirectoryIfMissing True ((C.unpack . O.name) t)
         handleEntries (O.entries t) 
       (O.CommitObj c) -> throwError "Expecting tree, but received commit"
       (O.BlobObj b) -> throwError "Expecting tree, but received blob"

    where
      handleEntries [] = return ()
      handleEntries (e@(t, oId, oName):es) = 
        case t of
          O.TTree -> do
             extractTreeToDisk oId
             handleEntries es
          O.TBlob -> do
             ro <- readObjectFromFile oId
             case ro of
                 (O.BlobObj b') -> do
                   liftIO $ C.writeFile (C.unpack oName) (O.content b') 
                   handleEntries es
                 _              -> 
                   throwError "Found another object instead of blob"


  getRepo = ask

  setHead b ref = do
    r <- getRepo
    liftIO $ C.writeFile (r ++ "/.hit/" ++ "HEAD") 
              (C.pack ("refs: refs/heads/" ++ b))

  updateBranchRef b cId = do
    r <- getRepo
    let branchPath = branchBuilder r b
    liftIO $ C.writeFile branchPath cId

  -- | Copy commit ID from HEAD to the new created ref b
  addBranch b = do
        r <- getRepo
        hr <- getHeadRef
        cId <- liftIO $ C.readFile (C.unpack hr)
        let branchPath = branchBuilder r b
        liftIO $ C.writeFile branchPath cId
        return hr


  getHeadRef = do
    r <- getRepo
    let headFile = r ++ ".hit/HEAD" 
    isFile <- liftIO $ doesFileExist headFile
    if isFile then do
      branch <- liftIO $ C.readFile headFile
      let maybePath = C.stripPrefix (C.pack "refs: ") branch
      case maybePath of 
        Just p  -> return $ C.pack $ r ++ ".hit/" ++ C.unpack p
        Nothing -> throwError "Malformed HEAD file"
    else throwError "HEAD file does not exist"

  
  readRefs ref = do
    r <- getRepo
    exists <- liftIO $ doesDirectoryExist (r ++ "/.hit/refs/heads")
    if exists then do
      branches <- liftIO $ listDirectory (r ++ "/.hit/refs/heads")
      addRefs branches ref
    else throwError "refs/heads does not exist, no new refs added to store"
    where
        addRefs [] ref     = return ref
        addRefs (b:bs) ref = do
           r <- getRepo
           id <- liftIO $ C.readFile (r ++ "/.hit/refs/heads/" ++ b)
           addRefs bs (OS.addRef ref (C.pack b) id)
  
  repomappend x y = mappend <$> x <*> y 

getCommitParent ::  (RepoMonad m) => O.Commit -> m (Set O.Commit)
getCommitParent x = 
  case O.parents x of 
        [] -> return Set.empty
        (x:xs) ->
              if x == C.pack "" -- hacky way to check for base case
              then return Set.empty else do  
              singletons <- sequence $ fmap getCommitObject (x:xs)
              return $ Set.unions singletons

getCommitObject :: (RepoMonad m) => O.ObjectId -> m (Set O.Commit)
getCommitObject objId = do
  commitObj <- readObjectFromFile objId  
  case commitObj of
    (O.CommitObj c) -> return $ Set.singleton c
    _ -> return Set.empty

createEmptyRepo :: (RepoMonad m, MonadIO m) => m ()
createEmptyRepo = do
  repo <- getRepo
  liftIO $ Prelude.mapM_ (createDirectoryIfMissing True) (folders repo)
  liftIO $ Prelude.writeFile (repo ++ "/.hit/HEAD") "refs: refs/heads/master"
  return ()
  where folders repo = [repo ++ "/.hit/objects", repo ++ "/.hit/refs/heads"]

branchBuilder r b = 
  if (r ++ ".hit/refs/heads/") `isPrefixOf` b then
    b
  else
    r ++ "/.hit/refs/heads/" ++ b



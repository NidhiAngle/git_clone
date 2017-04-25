module IOInterface where

import qualified Objects as O
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Lazy as B
import qualified ObjectStore as OS
import qualified Data.ByteString.Char8 as C
import qualified GitRevisions as GR
import qualified RepoMonad as RM
import Data.List (sortOn)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Time.Clock as DT
import qualified Data.Time.Format as DTF
import Control.Monad 
import Control.Monad.Except
import Control.Monad.Trans.Reader
import System.Directory (createDirectoryIfMissing, listDirectory,
                         doesDirectoryExist,doesFileExist)
import System.FilePath (splitFileName)

type Author = C.ByteString
type Message = C.ByteString


commit :: (RM.RepoMonad m, MonadIO m) => 
          [OS.Ref] -> Author -> Message -> m O.ObjectId
commit refs a m = do
  r <- RM.getRepo
  objectIds <- commitDirectories r
  let tree = O.makeTree objectIds
  filename <- RM.writeObjectToFile tree
  utc <- liftIO DT.getCurrentTime
  let c = O.makeCommit refs filename a m utc
  RM.writeObjectToFile c


commitDirectories :: (RM.RepoMonad m, MonadIO m) => FilePath -> m [O.TreeEntry]
commitDirectories fp = do
    r <- RM.getRepo 
    filePaths <- liftIO $ listDirectory fp
    foldr commit' (return []) filePaths where
    commit' dir = liftM2 mappend (commitEachDirectory dir) 
    commitEachDirectory filepath = do
        entry <- commitDirectory filepath
        case entry of
          Just x  -> return [x]
          Nothing -> return []


commitDirectory :: (RM.RepoMonad m, MonadIO m) => FilePath -> m (Maybe O.TreeEntry)
commitDirectory filePath = do
  r <- RM.getRepo 
  if (head . snd . splitFileName) filePath == '.' then return Nothing else do
  isDirectory <- liftIO $ doesDirectoryExist filePath
  isFile <- liftIO $ doesFileExist filePath 
  case (isDirectory, isFile) of  
    (True, _) -> do 
        objectIds <- commitDirectories filePath
        let tree = O.makeTree objectIds
        filename <- RM.writeObjectToFile tree
        return $ Just (O.makeTreeEntryType, filename, C.pack filePath)
    (_, True) -> do
         contents <- liftIO $ C.readFile filePath
         let blob = O.makeBlob contents
         fileName <- RM.writeObjectToFile blob
         return $ Just (O.makeBlobEntryType, fileName, C.pack filePath)
    (_,_)  ->  return Nothing 


--ASSUMPTION NO REMOTES
--RM.readRefs OS.Repo    

createEmptyRepo :: (RM.RepoMonad m, MonadIO m) => m ()
createEmptyRepo = do
  repo <- RM.getRepo
  liftIO $ Prelude.mapM_ (createDirectoryIfMissing True) (folders repo)
  liftIO $ Prelude.writeFile (repo ++ "/.hit/HEAD") "refs: refs/heads/master"
  return ()
  where folders repo = [repo ++ "/.hit/objects", repo ++ "/.hit/refs/heads"]

initialize ::  (RM.RepoMonad m, MonadIO m) => OS.RefStore -> m OS.RefStore
initialize ref = do
  r <- RM.getRepo
  dexists <- liftIO $ doesDirectoryExist (r ++ "/.hit") 
  if dexists then 
    RM.readRefs ref
  else do
    createEmptyRepo             
    return ref

commitPrep refMap msg = do 
  refMap'  <- RM.readRefs refMap
  head     <- RM.getHeadRef
  commitId <- commit [head] (C.pack "Brendon") (C.pack msg)
  setHdres <- RM.setHead "master" commitId
  return (refMap', commitId)

runIOExceptT :: ExceptT String IO a -> [IO ()] -> (a -> IO ()) -> IO ()
runIOExceptT m g f = do
        result <- runExceptT m
        case result of
            Right a -> f a
            Left e -> foldl (>>) (putStrLn e) g 

getLog :: (RM.RepoMonad m) => m (IO ())
getLog = do 
  headRef <- RM.getHeadRef
  headCommit <- RM.readObjectFromFile headRef
  case headCommit of
    (O.CommitObj c) -> do
                       commitLog <- GR.revParseTree [GR.RevId c] getCommitParent 
                       return $ printLogs commitLog 
    _               -> return $ putStrLn $ "Not a commit object" ++ 
                                            C.unpack headRef
  where
  printLogs logSet = mapM_ (print . show) (Set.toList logSet)
  

getCommitParent ::  (RM.RepoMonad m) => O.Commit -> m (Set O.Commit)
getCommitParent x = 
  case O.parents x of 
        [] -> return Set.empty
        (x:xs) ->
              if x == C.pack "" -- hacky way to check for base case
              then return Set.empty else do  
              singletons <- sequence $ fmap getCommitObject (x:xs)
              return $ Set.unions singletons

    
getCommitObject :: (RM.RepoMonad m) => O.ObjectId -> m (Set O.Commit)
getCommitObject objId = do
  commitObj <- RM.readObjectFromFile objId  
  case commitObj of
    (O.CommitObj c) -> return $ Set.singleton c
    _ -> return Set.empty

initRef :: (RM.RepoMonad m) => m OS.RefStore
initRef = do
  let ref = OS.createRef
  RM.readRefs ref


userInterface :: IO ()
userInterface = 
  runIOExceptT (runReaderT (initRef :: RM.RepoState OS.RefStore)  "./") [] go
  where
  go :: OS.RefStore -> IO()
  go refMap = do
    liftIO $ Prelude.putStr "hit> "
    str <- liftIO  Prelude.getLine
    case str of
      "init"   -> runIOExceptT
                    (runReaderT (initialize refMap :: 
                                  RM.RepoState OS.RefStore) "./")
                    [go refMap] 
                    ((putStrLn "Initialized hit repo" >>) . go)
      "commit" -> do
                  putStrLn "Please enter a commit message" 
                  msg    <- getLine 
                  runIOExceptT 
                    (runReaderT (commitPrep refMap msg :: 
                                 RM.RepoState (OS.RefStore, O.ObjectId)) "./")
                    [go refMap] 
                    (\(r,c) -> putStrLn ("Commit ID: " ++ C.unpack c) >> go r)
      "log"    ->  runIOExceptT 
                    (runReaderT (getLog :: RM.RepoState (IO ())) "./") 
                    [go refMap] 
                    (>> go refMap)
      "branch" -> do
                  putStrLn "Enter branch name"
                  branchName <- getLine
                  putStrLn "okay" >> go refMap
      "exit"   -> return ()
      _        -> Prelude.putStrLn "Unrecognized command" >> go refMap


  -- go refMap = do
  --    liftIO $ Prelude.putStr "hit> "
  --    str <- liftIO $ Prelude.getLine
  --    case str of
  --      "init"   -> do
  --                  r <- runExceptT (initialize "./" refMap) 
  --                  case r of
  --                    Left str      -> putStr str
  --                    Right refMap  -> putStrLn ("Initialized hit repo") >> go refMap
  --      "commit" -> do
  --                  putStrLn ("Please enter a commit message")
  --                  msg    <- Prelude.getLine 
  --                  result <- runExceptT $ commitPrep refMap msg
  --                  case result of
  --                    Right (r,c) -> putStrLn ("Commit ID: " ++ (C.unpack c)) >> go r
  --                    Left str    -> putStrLn str
  --      "exit" -> return ()
  --      _      -> Prelude.putStrLn "Unrecognized command" >> go refMap

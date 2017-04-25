module IOInterface (
  writeObjectToFile,
  readObjectFromFile
) where

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
import System.Directory (createDirectoryIfMissing, listDirectory,
                         doesDirectoryExist,doesFileExist)
import System.FilePath (splitFileName)
import RepoMonad
type Author = C.ByteString
type Message = C.ByteString


commit ::  (RepoMonad m, Monad m, MonadIO m) => 
           OS.Repo -> [OS.Ref] -> Author -> Message -> m O.ObjectId
commit r refs a m = do
  objectIds <- commitDirectories r r
  let tree = O.makeTree objectIds
  filename <- RM.writeObjectToFile r tree
  utc <- liftIO DT.getCurrentTime
  let c = O.makeCommit refs filename a m utc
  RM.writeObjectToFile r c


commitDirectories :: (RepoMonad m, Monad m, MonadIO m) => FilePath -> OS.Repo -> 
                     m [(O.EntryType, O.ObjectId, O.ObjectName)]
commitDirectories fp r = do 
    filePaths <- liftIO $ listDirectory fp
    foldr commit' (return []) filePaths where
    commit' dir acc = commitEachDirectory dir r `repomappend` acc
    commitEachDirectory filepath r = do
        entry <- commitDirectory filepath r
        case entry of
          Just x  -> return [x]
          Nothing -> return []


commitDirectory :: (RepoMonad m, Monad m, MonadIO m ) => FilePath -> OS.Repo -> 
                   m (Maybe (O.EntryType, O.ObjectId, O.ObjectName))
commitDirectory filePath r = 
  if (head . snd . splitFileName) filePath == '.' then return Nothing else do
  isDirectory <- liftIO $ doesDirectoryExist filePath
  isFile <- liftIO $ doesFileExist filePath 
  case (isDirectory, isFile) of  
    (True, _) -> do 
        objectIds <- commitDirectories filePath r
        let tree = O.makeTree objectIds
        filename <- RM.writeObjectToFile r tree
        return $ Just (O.makeTreeEntryType, filename, C.pack filePath)
    (_, True) -> do
         contents <- liftIO $ C.readFile filePath
         let blob = O.makeBlob contents
         fileName <- RM.writeObjectToFile r blob
         return $ Just (O.makeBlobEntryType, fileName, C.pack filePath)
    (_,_)  ->  return Nothing 


--ASSUMPTION NO REMOTES
--readRefs OS.Repo    

createEmptyRepo :: OS.Repo -> IO ()
createEmptyRepo repo = do
  Prelude.mapM_ (createDirectoryIfMissing True) folders
  Prelude.writeFile (repo ++ "/.hit/HEAD") "refs: refs/heads/master"
  where folders = [repo ++ "/.hit/objects", repo ++ "/.hit/refs/heads"]

initialize :: (RepoMonad m, Monad m, MonadIO m) => 
              OS.Repo -> OS.RefStore -> m OS.RefStore
initialize r ref = do
  dexists <- liftIO $ doesDirectoryExist (r ++ "/.hit") 
  if dexists then 
--    liftIO $ Prelude.putStrLn "Reinitializing git directory"
    readRefs r ref
  else do
    liftIO $ createEmptyRepo r             
    return ref


commitPrep refMap msg = do 
  refMap'  <- readRefs "./" refMap
  head     <- getHeadRef "./"
  commitId <- commit "./" [head] (C.pack "Brendon") 
              (C.pack msg)
  setHdres <- setHead "./" "master" commitId
  return (refMap', commitId)


runIOExceptT :: ExceptT String IO a -> [IO ()] -> (a -> IO ()) -> IO ()
runIOExceptT m g f = do
        result <- runExceptT m
        case result of
            Right a -> f a
            Left e -> foldl (>>) (putStrLn e) g 

getLog :: OS.Repo -> OS.Ref -> IO ()
getLog repo headRef = 
  runIOExceptT (RM.readObjectFromFile repo headRef) [] processHead where
  processHead hc = 
    case hc of
      (O.CommitObj c) -> runIOExceptT 
                           (GR.revParseTree [GR.RevId c] (getCommitParent repo)) 
                          [] printLogs
      _               -> putStrLn $ "Not a commit object" ++ C.unpack headRef
  printLogs logSet = mapM_ (print . show) (Set.toList logSet)
  

getCommitParent ::  OS.Repo -> O.Commit -> ExceptT String IO (Set O.Commit)
getCommitParent repo x =
  case O.parents x of 
        []       -> return Set.empty
        xs       -> liftIO $ fmap Set.unions $ sequence $ 
                      fmap (getCommitObject repo) xs

    
getCommitObject :: OS.Repo -> O.ObjectId -> IO (Set O.Commit)
getCommitObject repo objId = do
        commitObj <- runExceptT (RM.readObjectFromFile repo objId :: 
                                  ExceptT String IO O.Object)
        case commitObj of
                (Right (O.CommitObj c)) -> return $ Set.singleton c
                _ -> return Set.empty

initRef :: OS.Repo -> IO OS.RefStore
initRef repo = do
  let ref = OS.createRef
  res <- runExceptT (RM.readRefs repo ref :: ExceptT String IO OS.RefStore)
  case res of
    Right r -> return r
    Left _  -> return ref 


userInterface :: IO ()
userInterface = do
  refMap <- (initRef "./")
  go refMap where
  go :: OS.RefStore -> IO()
  go refMap = do
    liftIO $ Prelude.putStr "hit> "
    str <- liftIO  Prelude.getLine
    case str of
      "init"   -> runIOExceptT (initialize "./" refMap) [go refMap] 
                    ((putStrLn "Initialized hit repo" >>) . go)
      "commit" -> do
                  putStrLn "Please enter a commit message" 
                  msg    <- Prelude.getLine 
                  runIOExceptT (commitPrep refMap msg) [go refMap] 
                    (\(r,c) -> putStrLn ("Commit ID: " ++ C.unpack c) >> go r)
      "log"  -> runIOExceptT (RM.readRefs "./" refMap) [go refMap] 
                  (\r -> runIOExceptT (RM.getHeadRef "./") [go refMap] 
                     (\h -> getLog "./" h >> go r)) 
      "exit" -> return ()
      _      -> Prelude.putStrLn "Unrecognized command" >> go refMap


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
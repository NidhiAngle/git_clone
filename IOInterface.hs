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
import Control.Monad.Trans.Reader
import System.Directory (createDirectoryIfMissing, listDirectory,
                         doesDirectoryExist,doesFileExist)
import System.FilePath (splitFileName)
import RepoMonad
type Author = C.ByteString
type Message = C.ByteString



--ASSUMPTION NO REMOTES
--readRefs OS.Repo    

createEmptyRepo :: (RepoMonad m, MonadIO m) => m ()
createEmptyRepo = do
  repo <- getRepo
  liftIO $ Prelude.mapM_ (createDirectoryIfMissing True) (folders repo)
  liftIO $ Prelude.writeFile (repo ++ "/.hit/HEAD") "refs: refs/heads/master"
  return ()
  where folders repo = [repo ++ "/.hit/objects", repo ++ "/.hit/refs/heads"]

initialize ::  (RepoMonad m, MonadIO m) => OS.RefStore -> m OS.RefStore
initialize ref = do
  r <- getRepo
  dexists <- liftIO $ doesDirectoryExist (r ++ "/.hit") 
  if dexists then 
    readRefs ref
  else do
    createEmptyRepo             
    return ref

commitPrep f refMap msg = do 
  refMap'  <- readRefs refMap
  head     <- getHeadRef
  commitId <- RM.commit f [head] (C.pack "Brendon") (C.pack msg)
  setHdres <- setHead "master" commitId
  return (refMap', commitId)

getLog :: (RepoMonad m) => m (IO ())
getLog = do 
  headRef <- getHeadRef
  headCommit <- RM.readObjectFromFile headRef
  case headCommit of
    (O.CommitObj c) -> do
                       commitLog <- GR.revParseTree [GR.RevId c] getCommitParent 
                       return $ printLogs commitLog 
    _               -> return $ putStrLn $ "Not a commit object" ++ 
                                            C.unpack headRef
  where
  printLogs logSet = mapM_ (print . show) (Set.toList logSet)
  

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
  commitObj <- RM.readObjectFromFile objId  
  case commitObj of
    (O.CommitObj c) -> return $ Set.singleton c
    _ -> return Set.empty

initRef :: IO OS.RefStore
initRef = do
  let ref = OS.createRef
  rs <- runExceptT $ runReaderT (RM.readRefs ref :: 
                                     RepoState OS.RefStore) findRepoPath
  case rs of 
    Right rs' -> return rs'
    Left _ -> return ref

findRepoPath :: FilePath
findRepoPath = "./"

userInterface :: IO ()
userInterface = do 
  rs <- initRef
  go rs
  where
  go :: OS.RefStore -> IO()
  go rs = do
    liftIO $ Prelude.putStr "hit> "
    str <- liftIO  Prelude.getLine
    case str of
      "init"   -> do
                  init <- runExceptT $ runReaderT (initialize rs :: 
                                                   RepoState OS.RefStore) findRepoPath
                  case init of 
                    Right rs' -> putStrLn "Initialized hit repo" >> go rs'
                    Left e -> putStrLn e >> go rs
      "commit" -> do
                  putStr "Please enter a commit message: " 
                  msg    <- getLine 
                  c <- runExceptT $ runReaderT (commitPrep RM.writeObjectToFile rs msg ::
                                     RepoState (OS.RefStore, O.ObjectId)) findRepoPath
                  case c of
                    Right (refMap', c') -> 
                        putStrLn ("Commit ID: " ++ C.unpack c') >> go refMap'
                    Left e -> putStrLn e >> go rs
      "log"    -> do 
                  log <- runExceptT $ runReaderT (getLog :: 
                                                  RepoState (IO ())) findRepoPath 
                  let msg = case log of 
                              Right log' -> log'
                              Left e -> putStrLn e
                  msg >> go rs
      "branch" -> do
                  putStr "Enter branch name: "
                  branch <- getLine
                  let branchName = C.pack branch
                  case OS.lookupRef branchName rs of
                    Just _ -> putStrLn (branch ++ " already exists") >> go rs 
                    Nothing -> do
                      ab <- runExceptT $ runReaderT (RM.addBranch branch ::
                                                     RepoState OS.Ref) "./"
                      case ab of
                        Right hr -> do 
                          let rs' = OS.addRef rs branchName hr 
                          putStrLn ("Successfully created " ++ branch) >> go rs'
                        Left e  -> putStrLn e >> go rs
      "checkout" -> do
                    putStr "Enter branch name: "
                    branch <- getLine
                    let branchName = C.pack branch
                    case OS.lookupRef branchName rs of
                      Just id -> do
                         b <- runExceptT $ runReaderT (isWorkingDirectoryDirty ::
                                                      RepoState Bool) findRepoPath
                         case b of 
                           Right dirty ->
                             if not dirty then do
                               co <- runExceptT $ runReaderT (switchToBranch id :: 
                                                             RepoState (IO ())) findRepoPath
                               case co of 
                                 Right co' -> co' >> go rs
                                 Left e -> putStrLn e >> go rs  
                             else putStrLn ("Some local files would be overwritten" ++
                                           " in checkout. Please commit first") >> go rs
                           Left e -> putStrLn e >> go rs
                      Nothing -> putStrLn ("branch " ++ branch ++ " does not exist") >> go rs 
      "exit"   -> return ()
      _        -> Prelude.putStrLn "Unrecognized command" >> go rs

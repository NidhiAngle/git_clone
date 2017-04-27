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
import MyDiff as D

type Author = C.ByteString
type Message = C.ByteString
 


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

commitPrep f refMap msg = do 
  refMap'    <- RM.readRefs refMap
  head       <- RM.getHeadRef
  commitId   <- RM.commit f [head] (C.pack "Brendon") (C.pack msg)
  RM.updateBranchRef (C.unpack head) commitId
  return (refMap', commitId)

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
  printLogs logSet = mapM_ func (Set.toList logSet)
  func = (\x -> putStrLn (C.unpack (O.toLineCommit x) ++ "\n~~~~"))

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

initRef :: OS.Repo -> IO OS.RefStore
initRef repo = do
  let ref = OS.createRef
  rs <- runExceptT $ runReaderT (RM.readRefs ref :: 
                                     RM.RepoState OS.RefStore) repo
  case rs of 
    Right rs' -> return rs'
    Left _ -> return ref

findRepoPath :: FilePath
findRepoPath = "./"

getBranchName :: IO String
getBranchName = putStr "Enter branch name: " >> getLine

userInterface :: String -> IO ()
userInterface repo = do 
  rs <- initRef repo
  go rs
  where
  go :: OS.RefStore -> IO()
  go rs = do
    liftIO $ Prelude.putStr "hit> "
    str <- liftIO  Prelude.getLine
    case str of
      "init"   -> do
                  init <- runExceptT $ runReaderT (initialize rs :: 
                                                   RM.RepoState OS.RefStore) repo
                  case init of 
                    Right rs' -> putStrLn "Initialized hit repo" >> go rs'
                    Left e -> putStrLn e >> go rs
      "commit" -> do
                  putStr "Please enter a commit message: " 
                  msg    <- getLine 
                  c <- runExceptT $ runReaderT (commitPrep RM.writeObjectToFile rs msg ::
                                     RM.RepoState (OS.RefStore, O.ObjectId)) repo
                  case c of
                    Right (refMap', c') -> 
                        putStrLn ("Commit ID: " ++ C.unpack c') >> go refMap'
                    Left e -> putStrLn e >> go rs
      "log"    -> do 
                  log <- runExceptT $ runReaderT (getLog :: 
                                                  RM.RepoState (IO ())) repo 
                  let msg = case log of 
                              Right log' -> log'
                              Left e -> putStrLn e
                  msg >> go rs
      "branch" -> do
                  branch <- getBranchName
                  let branchName = C.pack branch
                  case OS.lookupRef branchName rs of
                    Just _ -> putStrLn (branch ++ " already exists") >> go rs 
                    Nothing -> do
                      ab <- runExceptT $ runReaderT (RM.addBranch branch ::
                                                     RM.RepoState OS.Ref) repo
                      case ab of
                        Right hr -> do 
                          let rs' = OS.addRef rs branchName hr 
                          putStrLn ("Successfully created " ++ branch) >> go rs'
                        Left e  -> putStrLn e >> go rs
      "checkout" -> do
                    branch <- getBranchName
                    let branchName = C.pack branch
                    case OS.lookupRef branchName rs of
                      Just id -> do
                         b <- runExceptT $ runReaderT (RM.isWorkingDirectoryDirty ::
                                                      RM.RepoState Bool) repo
                         case b of 
                           Right dirty ->
                             if not dirty then do
                               co <- runExceptT $ runReaderT (RM.switchToBranch branch :: 
                                                             RM.RepoState ()) repo
                               case co of 
                                 Right _ -> go rs
                                 Left e -> putStrLn e >> go rs  
                             else putStrLn ("Some local files would be overwritten" ++
                                           " in checkout. Please commit first") >> go rs
                           Left e -> putStrLn e >> go rs
                      Nothing -> putStrLn ("branch " ++ branch ++ " does not exist") >> go rs 
      "diff -o"  -> do
                    putStr "Enter first objectId: " 
                    f1 <- getLine
                    putStr "Enter second objectId: "
                    f2 <- getLine
                    res <- runExceptT (runReaderT (D.diff (C.pack f1) (C.pack f2)) "./test")
                    case res of
                      Right str -> putStrLn str >> go rs
                      Left  str -> putStrLn str >> go rs 
      "diff -f"  -> do
                    putStr "Enter first file: " 
                    f1 <- getLine
                    putStr "Enter second file: "
                    f2 <- getLine
                    res <- runExceptT (runReaderT (D.diff f1 f2) "./test")
                    case res of
                      Right str -> putStrLn str >> go rs
                      Left  str -> putStrLn str >> go rs 
      "exit"   -> return ()
      _        -> Prelude.putStrLn "Unrecognized command" >> go rs

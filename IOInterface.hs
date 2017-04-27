module IOInterface where

import qualified Objects as O
import qualified Data.ByteString.Lazy as B
import qualified ObjectStore as OS
import qualified Data.ByteString.Char8 as C
import qualified RepoMonad as RM
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad 
import Control.Monad.Except
import Control.Monad.Trans.Reader
import System.Directory (createDirectoryIfMissing,doesDirectoryExist)
import System.FilePath (splitFileName, takeFileName)
import Merge as M
import MyDiff as D

 
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

-- | commitPrep and mprep called to handle commits and merges respectively

commitPrep :: RM.RepoMonad m => (O.Object -> m O.ObjectId)
     -> OS.RefStore -> String -> m (OS.RefStore, O.ObjectId)
commitPrep f refMap msg = do 
  refMap'    <- RM.readRefs refMap
  head       <- RM.getHeadRef
  let bname = C.pack (takeFileName (C.unpack head)) 
  commitId   <- RM.commit f [head] (C.pack "Brendon") (C.pack msg)
  RM.updateBranchRef (C.unpack head) commitId
--  return (refMap' , commitId)
  return (OS.addRef refMap' bname commitId, commitId)

mprep:: String -> OS.RefStore -> ReaderT OS.Repo (ExceptT String IO) ()
mprep b2 rs= do
  path <- RM.getHeadRef
  let b1 = takeFileName (C.unpack path)
  case (OS.lookupRef (C.pack b1) rs, OS.lookupRef (C.pack b2) rs) of
    (Just i1, Just i2) -> do 
          i3 <- M.merger i1 i2
          _  <- RM.updateBranchRef b1 i3
          _  <- RM.switchToBranch b1
          return ()
    (_,_)  -> throwError "Couldnt find the branch id!"

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
                  init <- runExceptT $ 
                            runReaderT 
                              (initialize rs :: RM.RepoState OS.RefStore) 
                              repo
                  case init of 
                    Right rs' -> putStrLn "Initialized hit repo" >> go rs'
                    Left e -> putStrLn e >> go rs
      "commit" -> do
                  putStr "Please enter a commit message: " 
                  msg    <- getLine 
                  c <- runExceptT $ 
                         runReaderT 
                           (commitPrep RM.writeObjectToFile rs msg ::
                             RM.RepoState (OS.RefStore, O.ObjectId)) 
                           repo
                  case c of
                    Right (refMap', c') -> 
                        putStrLn ("Commit ID: " ++ C.unpack c') >> go refMap'
                    Left e -> putStrLn e >> go rs
      "log"    -> do 
                  log <- runExceptT $ runReaderT (RM.getLog :: 
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
                         b <- runExceptT $ runReaderT 
                                             (RM.isWorkingDirectoryDirty ::
                                              RM.RepoState Bool) repo
                         case b of 
                           Right dirty ->
                             if not dirty then do
                               co <- runExceptT $ runReaderT 
                                                    (RM.switchToBranch branch :: 
                                                     RM.RepoState ()) repo
                               case co of 
                                 Right _ -> go rs
                                 Left e -> putStrLn e >> go rs  
                             else putStrLn 
                                  ("Some local files would be overwritten" ++
                                   " in checkout. Please commit first") >> go rs
                           Left e -> putStrLn e >> go rs
                      Nothing -> putStrLn 
                                 ("branch " ++ branch ++ " does not exist") 
                                 >> go rs 
      "diff -o"  -> do
                    putStr "Enter first objectId: " 
                    f1 <- getLine
                    putStr "Enter second objectId: "
                    f2 <- getLine
                    res <- runExceptT $
                             runReaderT (D.diff (C.pack f1) (C.pack f2)) repo
                    case res of
                      Right str -> putStrLn str >> go rs
                      Left  str -> putStrLn str >> go rs 
      "diff -f"  -> do
                    putStr "Enter first file: " 
                    f1 <- getLine
                    putStr "Enter second file: "
                    f2 <- getLine
                    res <- runExceptT $ runReaderT (D.diff f1 f2) repo
                    case res of
                      Right str -> putStrLn str >> go rs
                      Left  str -> putStrLn str >> go rs
      "merge"    -> do
                    putStr "Enter branch to merge with: " 
                    b2 <- getLine
                    r1 <- runExceptT (runReaderT (mprep b2 rs) repo)
                    case r1 of
                      Right _ -> go rs
                      Left str -> putStrLn str >> go rs 
      "exit"     -> void (print rs)
      "ref"      -> print rs >> go rs
      _          -> Prelude.putStrLn "Unrecognized command" >> go rs


module IOInterface (
  writeObjectToFile,
  readObjectFromFile
) where

import qualified Objects as O
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Lazy as B
import qualified ObjectStore as OS
import qualified Data.ByteString.Char8 as C
import Control.Monad 
import Control.Monad.Except
import System.Directory (createDirectoryIfMissing, listDirectory,
                         doesDirectoryExist,doesFileExist)
import System.FilePath (splitFileName)
import RepoMonad
type Author = C.ByteString
type Message = C.ByteString

commit ::  (RepoMonad m, Monad m, MonadIO m ) => 
           OS.Repo -> [OS.Ref] -> Author -> Message -> m O.ObjectId
commit r refs a m = do
  objectIds <- commitDirectories r r
  let tree = O.makeTree objectIds
  filename <- writeObjectToFile r tree
  let c = O.makeCommit refs filename a m
  writeObjectToFile r c

commitDirectories :: (RepoMonad m, Monad m, MonadIO m ) => FilePath -> OS.Repo -> 
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
        filename <- writeObjectToFile r tree
        return $ Just (O.makeTreeEntryType, filename, C.pack filePath)
    (_, True) -> do
         contents <- liftIO $ C.readFile filePath
         let blob = O.makeBlob contents
         fileName <- writeObjectToFile r blob
         return $ Just (O.makeBlobEntryType, fileName, C.pack filePath)
    (_,_)  ->  return Nothing 


--ASSUMPTION NO REMOTES
--readRefs OS.Repo    

createEmptyRepo :: OS.Repo -> IO ()
createEmptyRepo repo = do
  Prelude.mapM_ (createDirectoryIfMissing True) folders
  Prelude.writeFile (repo ++ "/.hit/HEAD") "refs: refs/heads/master"
  where folders = [repo ++ "/.hit/objects", repo ++ "/.hit/refs/heads"]

initialize ::(RepoMonad m, Monad m, MonadIO m) => OS.Repo -> OS.RefStore -> m OS.RefStore
initialize r ref = do
  dexists <- liftIO $ doesDirectoryExist (r ++ "/.hit") 
  if dexists then do
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

userInterface :: IO ()
userInterface = go (OS.createRef) where
  go :: OS.RefStore -> IO()
  go refMap = do
    liftIO $ Prelude.putStr "hit> "
    str <- liftIO $ Prelude.getLine
    case str of
      "init"   -> do
                  r <- runExceptT (initialize "./" refMap) 
                  case r of
                    Left str      -> putStr str
                    Right refMap  -> putStrLn ("Initialized hit repo") >> go refMap
      "commit" -> do
                  putStrLn ("Please enter a commit message")
                  msg    <- Prelude.getLine 
                  result <- runExceptT $ commitPrep refMap msg
                  case result of
                    Right (r,c) -> putStrLn ("Commit ID: " ++ (C.unpack c)) >> go r
                    Left str    -> putStrLn str
      "exit" -> return ()
      _      -> Prelude.putStrLn "Unrecognized command" >> go refMap



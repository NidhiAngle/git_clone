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
import System.Directory (createDirectoryIfMissing, listDirectory,
                         doesDirectoryExist,doesFileExist)
import System.FilePath (splitFileName)

type Author = C.ByteString
type Message = C.ByteString

commit :: OS.Repo -> [OS.Ref] -> Author -> Message -> IO O.ObjectId
commit r refs a m = do
  objectIds <- commitDirectories r r
  let tree = O.makeTree objectIds
  filename <- writeObjectToFile r tree
  let c = O.makeCommit refs filename a m
  writeObjectToFile r c

commitDirectories :: FilePath -> OS.Repo -> 
                     IO [(O.EntryType, O.ObjectId, O.ObjectName)]
commitDirectories fp r = do 
    filePaths <- listDirectory fp
    foldr commit' (return []) filePaths where
    commit' dir acc = commitEachDirectory dir r `mappend` acc
    commitEachDirectory filepath r = do
        entry <- commitDirectory filepath r
        case entry of
             Just x -> return [x] 
             Nothing -> return []

commitDirectory :: FilePath -> OS.Repo -> 
                   IO (Maybe (O.EntryType, O.ObjectId, O.ObjectName))
commitDirectory filePath r = 
  if (head . snd . splitFileName) filePath == '.' then return Nothing else do
  isDirectory <- doesDirectoryExist filePath
  isFile <- doesFileExist filePath 
  case (isDirectory, isFile) of  
    (True, _) -> do 
        objectIds <- commitDirectories filePath r
        let tree = O.makeTree objectIds
        filename <- writeObjectToFile r tree
        return $ Just (O.makeTreeEntryType, filename, C.pack filePath)
    (_, True) -> do
         contents <- C.readFile filePath
         let blob = O.makeBlob contents
         fileName <- writeObjectToFile r blob
         return $ Just (O.makeBlobEntryType, fileName, C.pack filePath)
    (_,_)  ->  return Nothing 


--ASSUMPTION NO REMOTES
--readRefs OS.Repo
readRefs repo ref = do
  branches <- listDirectory (repo ++ "/.hit/refs/heads")
  addRefs branches
  where
    addRefs []     = return ref
    addRefs (b:bs) = do
                       id <- C.readFile (repo ++ "/.hit/refs/heads/" ++ b)
                       return $ OS.addRef ref (C.pack b) id    

createEmptyRepo :: OS.Repo -> IO ()
createEmptyRepo repo = Prelude.mapM_ (createDirectoryIfMissing True) folders
    where folders = [repo ++ "/.hit/objects", repo ++ "/.hit/refs/heads"]

initialize :: OS.Repo -> OS.RefStore -> IO OS.RefStore
initialize r ref = do
  dexists <- doesDirectoryExist (r ++ "/.hit") 
  if dexists then do
    Prelude.putStrLn "Reinitializing git directory"
    readRefs r ref
  else do
    createEmptyRepo r
    Prelude.putStrLn "Initializing git directory"
    Prelude.writeFile (r ++ "/.git/refs/HEAD") "refs: refs/heads/master"
    return ref

-- First one else where??
setHead :: OS.Repo -> OS.Branch -> OS.Ref -> IO ()
setHead r b ref = do
  C.writeFile (r ++ "/.hit/refs/heads/" ++ b) ref 
  C.writeFile (r ++ "/.hit/" ++ "HEAD") (C.pack ("refs/heads/" ++ b))

getHeadRef :: OS.Repo -> IO (Maybe OS.Ref)
getHeadRef r = do
  isFile <- doesFileExist $ r ++ ".hit/HEAD" 
  if isFile then do
  branch <- C.readFile (r ++ ".hit/HEAD")
  let branchPath = r ++ ".hit/" ++ C.unpack branch
  isFile' <- doesFileExist branchPath 
  if isFile' 
  then do
    branchName <- C.readFile branchPath 
    return $ Just branchName
  else return Nothing
  else return Nothing

userInterface :: IO()
userInterface = go OS.createRef where
  go refMap = do
    Prelude.putStr "hit> "
    str <- Prelude.getLine
    case str of
      "init" -> initialize "./" refMap >>= go
      "commit" -> do
                  refMap' <- readRefs "./" refMap
                  maybeHead <- getHeadRef "./"
                  let head = case maybeHead of
                               Just h -> [h] 
                               Nothing -> []
                  commitId <- commit "./" head (C.pack "Brendon") 
                              (C.pack "default msg")
                  setHead "./" "master" commitId
                  putStrLn ("Commit ID: " ++ C.unpack commitId) >> go refMap'
      "exit" -> do 
                 print refMap
                 return ()
      _      -> Prelude.putStrLn "Unrecognized command" >> go refMap


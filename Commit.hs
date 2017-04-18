module Commit where

import qualified Objects as O
import qualified ObjectStore as OS
import qualified IOInterface as IOI

import qualified Data.ByteString.Char8 as C

import System.Directory (listDirectory,doesDirectoryExist,doesFileExist)
import System.FilePath (splitFileName)

type Ref = O.ObjectId
type Author = C.ByteString
type Message = C.ByteString

commit :: OS.Repo -> Ref -> Author -> Message -> IO O.ObjectId
commit r head a m = do
  objectIds <- commitDirectories r r
  let tree = O.makeTree objectIds
  filename <- IOI.writeObjectToFile r tree
  let c = O.makeCommit [head] filename a m
  IOI.writeObjectToFile r c

commitDirectories :: FilePath -> OS.Repo -> IO [(O.EntryType, O.ObjectId, O.ObjectName)]
commitDirectories fp r = do 
    filePaths <- listDirectory fp
    foldr (\dir acc -> commitEachDirectory r dir `mappend` acc ) (return []) filePaths where
    commitEachDirectory r filepath = do
        entry <- commitDirectory r filepath 
        case entry of
             Just x -> return [x] 
             Nothing -> return []

commitDirectory :: FilePath -> OS.Repo -> IO (Maybe (O.EntryType, O.ObjectId, O.ObjectName))
commitDirectory filePath r = 
  if (head . snd . splitFileName) filePath == '.' then return Nothing else do
  isDirectory <- doesDirectoryExist filePath
  isFile <- doesFileExist filePath 
  case (isDirectory, isFile) of  
    (True, _) -> do 
        objectIds <- commitDirectories filePath r
        let tree = O.makeTree objectIds
        filename <- IOI.writeObjectToFile r tree
        return $ Just (O.makeTreeEntryType, filename, C.pack filePath)
    (_, True) -> do
         contents <- C.readFile filePath
         let blob = O.makeBlob contents
         fileName <- IOI.writeObjectToFile r blob
         return $ Just (O.makeBlobEntryType, fileName, C.pack filePath)
    (_,_)  ->  return Nothing 

main = commit "/Users/brendonlavernia/Documents/Classes/CIS552/project/code/test" (C.pack "dfasdfdsa") (C.pack "Brendon") (C.pack "first commit")

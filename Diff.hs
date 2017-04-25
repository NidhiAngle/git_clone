{-# LANGUAGE FlexibleInstances #-}
module MyDiff where
import Objects as O
import ObjectStore as OS
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import RepoMonad
import System.Directory (listDirectory,doesFileExist)
import System.FilePath
import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Char8 as C
import RepoMonad as RM  

class MyDiff a where
    diff :: (RepoMonad b, MonadIO b) => a -> a -> b String

instance MyDiff [Char] where
-- for Filepath
    diff f1 f2 =  do
        file1 <- liftIO $ readFile f1
        file2 <- liftIO $ readFile f2
        let
          lines1 = lines file1
          lines2 = lines file2
        return $ ppDiff $ getGroupedDiff lines1 lines2


instance MyDiff O.Object where

    diff (BlobObj b1) (BlobObj b2) = do
        let c1 = lines $ show $ toLineBlob b1
        let c2 = lines $ show $ toLineBlob b2
        return $ ppDiff $ getGroupedDiff c1 c2

    diff o1@(CommitObj c1) o2@(CommitObj c2) = do
        let 
          (i1,x) = OS.hashContent o1
          (i2,y) = OS.hashContent o2
        liftIO $ putStrLn $ "Commit 1:" ++ (C.unpack i1)
        liftIO $ putStrLn $ "Commit 2:" ++ (C.unpack i2)
        liftIO $ putStrLn ""
        diff (O.getTreeFromCommit c1) (O.getTreeFromCommit c2)

    diff o1@(TreeObj t1) o2@(TreeObj t2) = do
        let 
            (i1, c1) = OS.hashContent o1
            (i2, c2) = OS.hashContent o2
        liftIO $ putStrLn $ "Tree 1:" ++ (C.unpack i1)
        liftIO $ putStrLn $ "Tree 2:" ++ (C.unpack i2)
        liftIO $ putStrLn ""
        let 
            e1s = sortBy sorter (O.getEntries t1) 
            e2s = sortBy sorter (O.getEntries t2)
        diff e1s e2s
        where sorter (e1,e2,e3) (f1,f2,f3) =
        	compare (e1,e3) (f1,f3)
-- Compares first on type then on file. Sort entries with all tree first
instance MyDiff [TreeEntry] where
   
    diff (O.TTree, id, name) (O.TBlob, _, _) = do
        
    diff  x@((e1,e2,e3):es) y@((f1,f2,f3):fs) = do
    	if (x == y) then do
    		diff e2 f2
    	else if (x>y)


instance MyDiff O.ObjectId where 

   diff id1 id2 = do
   	o1 <- 


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
import Data.List (sortBy)



class MyDiff a where
    diff :: (RepoMonad b, MonadIO b) => a -> a -> b String
    
instance MyDiff [Char] where
-- for Filepath
    diff f1 f2 =  do
      exists1 <- liftIO $ doesFileExist f1
      exists2 <- liftIO $ doesFileExist f2
      if (exists1 && exists2) then do
        file1 <- liftIO $ readFile f1
        file2 <- liftIO $ readFile f2
        let
          lines1 = lines file1
          lines2 = lines file2
        return $ ppDiff $ getGroupedDiff lines1 lines2
      else return $ "File does not exist"
      

instance MyDiff O.Object where

    diff o1@(BlobObj b1) o2@(BlobObj b2) = do
      let 
          (i1, c1) = OS.hashContent o1
          (i2, c2) = OS.hashContent o2
          intro    = "\nBlob 1:" ++ (chopId (C.unpack i1)) ++ "\n" ++
                       "Blob 2:" ++ (chopId (C.unpack i2)) ++ "\n"
      let c1 = Prelude.lines $ show $ toLineBlob b1
      let c2 = Prelude.lines $ show $ toLineBlob b2
      return $ intro ++ (ppDiff $ getGroupedDiff c1 c2)
      

    diff o1@(TreeObj t1) o2@(TreeObj t2) = do
      let 
        (i1, c1) = OS.hashContent o1
        (i2, c2) = OS.hashContent o2
        intro    = "\nTree 1:" ++ (chopId (C.unpack i1)) ++ "\n" ++
                     "Tree 2:" ++ (chopId (C.unpack i2)) ++ "\n" 
        e1s = sortBy sorter (O.getEntries t1) 
        e2s = sortBy sorter (O.getEntries t2)
      ((++) intro) <$> diff e1s e2s
      where 
        sorter (e1,e2,e3) (f1,f2,f3) = compare (e1,e3) (f1,f3)
    
    diff o1@(CommitObj c1) o2@(CommitObj c2) = do
      let 
        (i1, x) = OS.hashContent o1
        (i2, y) = OS.hashContent o2
        intro    = "\nCommit 1:" ++ (chopId (C.unpack i1)) ++ "\n" ++
                   "\nCommit 2:" ++ (chopId (C.unpack i2)) ++ "\n" 
      ((++) intro) <$> diff (getTreeFromCommit c1) (getTreeFromCommit c2)

    diff o1 o2 = do
      let (i1, c1) = OS.hashContent o1
          (i2, c2) = OS.hashContent o2
      return $ "\nCannot compare obj "++chopId (C.unpack(i1))++","++chopId(C.unpack(i2))

-- -- -- Compares first on type then on file. Sort entries with all tree first
instance MyDiff [TreeEntry] where
   
   diff e@((e1,e2,e3):es) f@((f1,f2,f3):fs) = do
      if (e == f) then
        ((++) ("\nNo change in "++C.unpack(e3)++","++C.unpack(f3)))
        <$> diff es fs
      else if (e1 == f1 && e3 == f3) then
        let str = ((++) ("\n~~Changes in " ++ C.unpack e3 ++","++ C.unpack f3++"\n")) <$> diff e2 f2 in
        (++) <$> str <*> diff es fs
      else if (e < f) then
        (++) <$> display "First" e2 e3 <*> diff es f
              else -- First is a blob, second is a tree
        (++) <$> display "Second" f2 f3 <*> diff e fs
   
   diff [] f = do
     Prelude.foldl x (return "") f where
        x = (\b (f1,f2,f3) -> (++) <$> b <*> display "Second" f2 f3)

   diff f [] = do
     Prelude.foldl x (return "") f where
        x = (\b (f1,f2,f3) -> (++) <$> b <*> display "First" f2 f3)


instance MyDiff O.ObjectId where 

   diff id1 id2 = do
     o1 <- readObjectFromFile id1
     o2 <- readObjectFromFile id2
     diff o1 o2

-- -------------------
display :: RepoMonad m => [Char] -> ObjectId -> C.ByteString -> m [Char]
display str id name = do
  obj <- readObjectFromFile id
  case obj of 
    (TreeObj tree) -> do
      return $ "\n~~New folder in "++str++": "++ (C.unpack name) ++ "\n" ++
              (show (toLineTree tree))
    (BlobObj blob) -> do
      return $ "\n~~New file in "++str++": "++ (C.unpack name) ++ "\n" ++
             (show (toLineBlob blob))
    (CommitObj commit) -> do
      return "\nWhy is a commit a tree entry?"

chopId :: String -> String
chopId (a:(b:(c:(d:(e:es))))) = a:(b:(c:(d:[e])))    
{-# LANGUAGE FlexibleInstances #-}
module Merge where
import Data.List (sortBy)
import ObjectStore as OS
import Objects as O
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.ByteString.Char8 as C
import RepoMonad 
import Control.Monad
import Control.Monad.Except
import Data.Set
import qualified Data.Time.Clock as DT



class Merge a where
  merge :: (RepoMonad b, MonadIO b) => a -> a -> b a


instance Merge O.Object where

  merge o1@(BlobObj b1) o2@(BlobObj b2) = do
    let 
      (i1, c1) = OS.hashContent o1
      (i2, c2) = OS.hashContent o2
    if i1 == i2 then
      return o1
    else do
      let c1 = Prelude.lines $ show $ toLineBlob b1
          c2 = Prelude.lines $ show $ toLineBlob b2
          newblob = makeBlob $ C.pack $ Prelude.foldl helperMerge "" (getDiff c1 c2)
      _ <- writeObjectToFile newblob
      return newblob  
    where helperMerge str diff = case diff of
                                      First  x -> str ++ "\nFIRST: " ++ x
                                      Second x -> str ++ "\nSECOND: "++ x
                                      Both x y -> str ++ "\n" ++ x

  merge o1@(TreeObj t1) o2@(TreeObj t2) = do
    let 
      (i1, c1) = OS.hashContent o1
      (i2, c2) = OS.hashContent o2
    if i1 == i2 then
      return o1
    else do
      let 
        e1s = sortBy sorter (O.getEntries t1) 
        e2s = sortBy sorter (O.getEntries t2) 
      newTree <- (makeTree (getTreeName t1)) <$> merge e1s e2s
      _ <- writeObjectToFile newTree
      return newTree
    where 
      sorter (e1,e2,e3) (f1,f2,f3) = compare (e1,e3) (f1,f3)

  merge o1@(CommitObj c1) o2@(CommitObj c2) = do
    let 
      (i1, x) = OS.hashContent o1
      (i2, y) = OS.hashContent o2
    if i1 == i2 then
      return o1
    else do
      let 
        auth = mergea (getAuthor c1) (getAuthor c2)
        prnt = mergep (getParents c1) (getParents c2)
        msg  = (C.pack "Merge of ") `append` i1 `append` (C.pack " and ") `append` i2
      o1 <- readObjectFromFile (getTree c1)
      o2 <- readObjectFromFile (getTree c2)
      o3 <- merge o1 o2
      utc <- liftIO DT.getCurrentTime
      let (i, x) = hashContent o3 
          newCommit = makeCommit prnt i auth msg utc
      _ <- writeObjectToFile newCommit
      return newCommit
    where mergea a1 a2 = do
            let l1 = C.words a1
                l2 = C.words a2
            C.unwords $ (toList . fromList) (l1++l2)
          mergep p1 p2 = (toList . fromList) (p1++p2)
      

  merge o1 o2 = return o1

instance Merge [TreeEntry] where
   
   merge e@((e1,e2,e3):es) f@((f1,f2,f3):fs) = do
     if (e == f) then
       ((:) (e1,e2,e3)) <$> merge es fs
     else if (e1 == f1 && e3 == f3) then do
       o1 <- readObjectFromFile e2
       o2 <- readObjectFromFile f2
       o3 <- merge o1 o2
       let (i, x) = hashContent o3 
       ((:) (e1,i,e3)) <$> merge es fs
     else if (e < f) then
       ((:) (e1,e2,e3)) <$> merge es f
     else -- First is a blob, second is a tree
       ((:) (f1,f2,f3)) <$> merge e fs
   
   merge [] f = return f

   merge f [] = return f


instance Merge [Char] where
   merge f1 f2 =  do
     file1 <- liftIO $ Prelude.readFile "test/pianoMan.txt"
     file2 <- liftIO $ Prelude.readFile "test/pianoMan2.txt"
     let
       lines1 = Prelude.lines file1
       lines2 = Prelude.lines file2
     return $ Prelude.foldl helperMerge "" (getDiff lines1 lines2)  
     where helperMerge str diff = case diff of
                                    First  x -> str ++ "\nFIRST: " ++ x
                                    Second x -> str ++ "\nSECOND: "++ x
                                    Both x y -> str ++ "\n" ++ x

merger :: ObjectId -> ObjectId -> RepoState ObjectId
merger id1 id2 = do
       o1 <- readObjectFromFile id1
       o2 <- readObjectFromFile id2
       case (o1, o2) of
         (CommitObj a, CommitObj b) -> do
           o3 <- merge o1 o2
           let (i, x) = hashContent o3
           return i
         (_, _) -> throwError "Cannot merge non branches!"
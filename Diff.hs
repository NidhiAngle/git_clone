{-# LANGUAGE FlexibleInstances #-}
module MyDiff where
import Objects as O
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import RepoMonad
import System.Directory (listDirectory,doesFileExist)
import System.FilePath
import Control.Monad
import Control.Monad.Except


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

    diff (CommitObj c1) (CommitObj c2) = do
        


import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import RepoMonad
import System.Directory (listDirectory,doesFileExist)
import System.FilePath
-- main = do
--     file1 <- readFile "file1.txt"
--     file2 <- readFile "file2.txt"

--     let
--         lines1 = lines file1
--         lines2 = lines file2
--    print $ getDiff lines1 lines2
--    print $ getGroupedDiff lines1 lines2
--    putStrLn $ ppDiff $ getGroupedDiff lines1 lines2
--    putStrLn $ show $ prettyDiffs $ diffToLineRanges $ getGroupedDiff lines1 lines2


class Diff a where
    diff :: (RepoMonad b) => a -> a -> b()

instance Diff FilePath where

	diff :: (RepoMonad m ) => FilePath -> FilePath -> m()

import Objects as O
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput


class Merge m where
     merge :: (RepoMonad b, MonadIO b) => a -> a -> b ()


-- instance Merge O.Object where

-- 	merge o1@(BlobObj b1) o2@(BlobObj b2) = do


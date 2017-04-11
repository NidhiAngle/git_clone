{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module GitRevisions (Rev, revParseTree, CaretQualifier(Head,Exclamation,Index, 
                      Dash), RevArg(RevId,Exclude,XOR,Caret)) where 

import Data.Set (Set)
import qualified Data.Set as Set

class (Ord a, Show a) => Rev a where
  getParents :: a -> Set a

data CaretQualifier = Head | Exclamation | Index Int | Dash Int

{-
 - RevId = A
 - Exclude = ^A
 - XOR = A...B
 - Caret Head = A^@
 - Caret Exclamation = A^!
 - Caret Index x = A^x
 - Caret Dash x = A^-x
 - XOR (Exclude A) B = A..B 
 -}
data RevArg a = RevId a | 
                 Exclude (RevArg a) | 
                 XOR (RevArg a) (RevArg a) | 
                 Caret a CaretQualifier

-- | Returns all the ancestors of Rev x along with x
-- | !!! Make sure to not add parents multiple times
getAllAncestors :: (Rev a) => a -> Set a
getAllAncestors x = case Set.toList $ getParents x of
                      [] -> Set.empty
                      -- Change to Set.unions
                      xs -> foldr (\y acc -> getAllAncestors y `Set.union` acc) 
                              (Set.singleton x) xs 
{- 
 - Takes a list of Rev arguments and returns the matching set of Revs
 -}
revParseTree :: (Rev a) => [RevArg a] -> Set a
revParseTree [] = Set.empty
revParseTree (RevId x:xs) = getAllAncestors x `Set.union` revParseTree xs 
revParseTree (Exclude x:xs) = revParseTree xs `Set.difference` revParseTree [x]
revParseTree (XOR (Exclude rev1) rev2:xs) = 
  revParseTree $ Exclude rev1 : rev2 : xs
revParseTree (XOR rev1 rev2:xs) =
  let set1 = revParseTree [rev1]
      set2 = revParseTree [rev2]
  in (set1 `Set.union` set2) 
     `Set.difference` 
     (set1 `Set.intersection` set2) 
     `Set.union` 
     revParseTree xs
revParseTree (Caret x Head:xs) = 
  getAllAncestors x 
  `Set.difference` 
  Set.singleton x 
  `Set.union` 
  revParseTree xs
revParseTree (Caret x Exclamation:xs) = 
  let ys = foldl (\acc y -> Exclude (RevId y) : acc) 
            [RevId x] (Set.toList (getParents x))
  in revParseTree (ys ++ xs)
revParseTree (Caret x (Index idx):xs) = 
  Set.singleton (Set.toList (getParents x) !! idx) 
  `Set.union` 
  revParseTree xs
revParseTree (Caret x (Dash idx):xs) = 
  revParseTree $ XOR (Exclude (Caret x (Index idx))) (RevId x) : xs



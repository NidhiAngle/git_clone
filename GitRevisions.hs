{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module GitRevisions (revParseTree, CaretQualifier(Head,Exclamation,Index, 
                      Dash), RevArg(RevId,Exclude,XOR,Caret)) where 

import Data.Set (Set)
import qualified Data.Set as Set

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
getAllAncestors :: (Ord a, Show a) => a -> (a -> Set a) -> Set a
getAllAncestors x getParents = case Set.toList $ getParents x of
                      [] -> Set.empty
                      -- Change to Set.unions
                      xs -> foldr (\y acc -> getAllAncestors y getParents `Set.union` acc) 
                              (Set.singleton x) xs 
{- 
 - Takes a list of Rev arguments and returns the matching set of Revs
 -}
revParseTree :: (Ord a, Show a) => [RevArg a] -> (a -> Set a) -> Set a
revParseTree [] _  = Set.empty
revParseTree (RevId x:xs) getParents = getAllAncestors x getParents 
                                       `Set.union` 
                                       revParseTree xs getParents
revParseTree (Exclude x:xs) getParents = revParseTree xs getParents 
                                         `Set.difference` 
                                         revParseTree [x] getParents
revParseTree (XOR (Exclude rev1) rev2:xs) getParents = 
  revParseTree (Exclude rev1 : rev2 : xs) getParents
revParseTree (XOR rev1 rev2:xs) getParents =
  let set1 = revParseTree [rev1] getParents 
      set2 = revParseTree [rev2] getParents 
  in (set1 `Set.union` set2) 
     `Set.difference` 
     (set1 `Set.intersection` set2) 
     `Set.union` 
     revParseTree xs getParents 
revParseTree (Caret x Head:xs) getParents = 
  getAllAncestors x getParents 
  `Set.difference` 
  Set.singleton x 
  `Set.union` 
  revParseTree xs getParents 
revParseTree (Caret x Exclamation:xs) getParents = 
  let ys = foldl (\acc y -> Exclude (RevId y) : acc) 
            [RevId x] (Set.toList (getParents x))
  in revParseTree (ys ++ xs) getParents 
revParseTree (Caret x (Index idx):xs) getParents = 
  Set.singleton (Set.toList (getParents x) !! idx) 
  `Set.union` 
  revParseTree xs getParents 
revParseTree (Caret x (Dash idx):xs) getParents = 
  revParseTree (XOR (Exclude (Caret x (Index idx))) (RevId x) : xs) getParents 



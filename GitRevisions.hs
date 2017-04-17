{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module GitRevisions (revParseTree, Relation(..), CaretQualifier(..), RevArg(..)
                    ) where 

import Data.Set (Set)
import qualified Data.Set as Set

data CaretQualifier = Head | Exclamation | Dash Word

data Relation a = Ancestor a | Parent a

{-
 - DirectParents = A^2 = C
 - Ancestors =     A~2 = D
 -}

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
data RevArg a  = RevId a | 
                 Exclude (RevArg a) | 
                 XOR (RevArg a) (RevArg a) | 
                 Caret a CaretQualifier |
                 Ancestry a [Relation Word]

-- | Returns all the ancestors of Rev x along with x
-- | !!! Make sure to not add parents multiple times
getAllAncestors :: (Ord a, Show a) => a -> (a -> Set a) -> Set a -> Set a
getAllAncestors x f s =
  if Set.member x s
  then s
  else do
      let newSet = Set.insert x s
      case Set.toList $ f x of
         [] -> newSet
         -- Change to Set.unions
         xs -> foldr (\y acc -> getAllAncestors y f acc `Set.union` acc) 
                      newSet xs 
{- 
 - Takes a list of Rev arguments and returns the matching set of Revs
 -}
revParseTree :: (Ord a, Show a) => [RevArg a ] -> (a -> Set a) -> Set a
revParseTree [] _  = Set.empty

revParseTree (RevId x:xs) f = getAllAncestors x f Set.empty 
                                       `Set.union` 
                                       revParseTree xs f

revParseTree (XOR (Exclude rev1) rev2:xs) f = 
  revParseTree (Exclude rev1 : rev2 : xs) f

revParseTree (Exclude x:xs) f = revParseTree xs f 
                                `Set.difference`
                                case Set.toList (revParseTree [x] f) of
                                  [] -> Set.empty
                                  y:_ -> getAllAncestors y f Set.empty 

revParseTree (XOR rev1 rev2:xs) f =
  let set1 = revParseTree [rev1] f 
      set2 = revParseTree [rev2] f 
  in (set1 `Set.union` set2) 
     `Set.difference` 
     (set1 `Set.intersection` set2) 
     `Set.union` 
     revParseTree xs f 

revParseTree (Caret x Head:xs) f = 
  getAllAncestors x f Set.empty 
  `Set.difference` 
  Set.singleton x 
  `Set.union` 
  revParseTree xs f 

revParseTree (Caret x Exclamation:xs) f = 
  let ys = foldl (\acc y -> Exclude (RevId y) : acc) 
            [RevId x] (Set.toList (f x))
  in revParseTree (ys ++ xs) f 

revParseTree (Caret x (Dash idx):xs) f = 
  revParseTree (XOR (Exclude (Ancestry x [Parent idx])) (RevId x) : xs) f 

revParseTree (Ancestry x as:xs) f = 
  case traverseFamily x as f of
    Nothing -> Set.empty
    Just y -> Set.insert y (revParseTree xs f)

traverseFamily :: (Ord a, Show a) => 
                  a -> [Relation Word] -> (a -> Set a) -> Maybe a
traverseFamily x [] _ = Just x 
traverseFamily x (Ancestor i:as) f =
  if i <= 0
  then traverseFamily x as f
  else 
    case Set.toList (f x) of
      [] -> Nothing
      p:ps -> traverseFamily p (Ancestor (i-1):as) f
traverseFamily x (Parent i:as) f =
  traverseFamily ((x : Set.toList (f x)) !! fromIntegral i) as f


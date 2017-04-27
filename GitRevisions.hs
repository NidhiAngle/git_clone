{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}
{- |
 - | API similar to gitrevisions for traversing a tree
 - | and collecting elements given a list of commands.
 -}
module GitRevisions (
  revParseTree, 
  Relation(..), 
  CaretQualifier(..), 
  RevArg(..)
) where 

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (liftM2)

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
getAllAncestors :: (Ord a, Show a, Monad m) => a -> (a -> m (Set a)) -> m (Set a) -> m (Set a)
getAllAncestors x f ms = do
  s <- ms
  if Set.member x s
  then ms
  else do
      let newSet = return $ Set.insert x s
      parents <-  f x
      let ancestors = case Set.toList parents of
                        [] -> newSet
                        -- Change to Set.unions
                        xs -> foldr (\y acc -> liftM2 Set.union (getAllAncestors y f acc) acc ) 
                                     newSet xs 
      ancestors
{- 
 - Takes a list of Rev arguments and returns the matching set of Revs
 -}
revParseTree :: (Ord a, Show a, Monad m) => [RevArg a ] -> (a -> m (Set a)) -> m (Set a)
revParseTree [] _  = return Set.empty

revParseTree (RevId x:xs) f = do 
  ancestors <- getAllAncestors x f (return Set.empty)
  others <- revParseTree xs f
  return $ ancestors `Set.union` others 

revParseTree (XOR (Exclude rev1) rev2:xs) f = 
  revParseTree (Exclude rev1 : rev2 : xs) f

revParseTree (Exclude x:xs) f = 
  liftM2 Set.difference (revParseTree xs f) (excludeList x f) where
  excludeList x f = do
    ancestors <- revParseTree [x] f
    case Set.toList ancestors of
                      [] -> return Set.empty
                      y:_ -> getAllAncestors y f (return Set.empty)

revParseTree (XOR rev1 rev2:xs) f = do
  let set1 =  revParseTree [rev1] f 
      set2 = revParseTree [rev2] f
      remaining = revParseTree xs f 
      difference = liftM2 Set.difference (liftM2 Set.union set1 set2) 
                          (liftM2 Set.intersection set1 set2) 
  liftM2 Set.union difference (revParseTree xs f) 

revParseTree (Caret x Head:xs) f = 
  liftM2 Set.union (liftM2 Set.difference (getAllAncestors x f (return Set.empty)) 
                                          (return $ Set.singleton x)) 
                   (revParseTree xs f)

revParseTree (Caret x Exclamation:xs) f = do
  parents <- f x 
  let ys = foldl (\acc y -> Exclude (RevId y) : acc) 
            [RevId x] (Set.toList parents)
  revParseTree (ys ++ xs) f 

revParseTree (Caret x (Dash idx):xs) f = 
  revParseTree (XOR (Exclude (Ancestry x [Parent idx])) (RevId x) : xs) f 

revParseTree (Ancestry x as:xs) f = do 
  familyMember <- traverseFamily x as f
  let result = case Set.toList familyMember of
                  [] -> return Set.empty
                  y:_ -> liftM2 Set.union (return familyMember) (revParseTree xs f)
  result

traverseFamily :: (Ord a, Show a, Monad m) => 
                  a -> [Relation Word] -> (a -> m (Set a)) -> m (Set a)
traverseFamily x [] _ = return $ Set.singleton x 
traverseFamily x (Ancestor i:as) f =
  if i <= 0
  then traverseFamily x as f
  else do
    parents <- f x
    case Set.toList parents of
      [] -> return Set.empty
      p:ps -> traverseFamily p (Ancestor (i-1):as) f
traverseFamily x (Parent i:as) f = do
  parents <- f x
  traverseFamily ((x : Set.toList parents) !! fromIntegral i) as f


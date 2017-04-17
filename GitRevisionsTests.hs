module GitRevisionsTests where

import GitRevisions
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
  classify,  maxSuccess, listOf, resize, scale, (==>))

data Node = N { getName :: String, getParents :: [Node] }
                  deriving (Read)

instance Eq Node where
  (==) n1 n2 = getName n1 == getName n2 && getParents n1 == getParents n2

instance Ord Node where
  compare n1 n2 = getName n1 `compare` getName n2

instance Show Node where
  show n = show ("(Node " ++ getName n ++ ", Children: ") ++ 
    showParents (getParents n) ++ ")" where
      showParents = foldr (\x acc -> show x ++ acc) "" 

main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()

{-
Tree test structure
G   H   I   J
 \ /     \ /
  D   E   F
   \  |  / \
    \ | /   |
     \|/    |
      B     C
       \   /
        \ /
         A
-}

g = N "G" []
h = N "H" []
i = N "I" []
j = N "J" []
d = N "D" [g, h]
e = N "E" []
f = N "F" [i,j]
b = N "B" [d,e,f]
c = N "C" [f]
a = N "A" [b,c]

getParentSet :: Node -> Set Node
getParentSet = Set.fromList . getParents

allTests :: Test
allTests = TestList [simpleRangeTests, parentTests] 

parentTests :: Test
parentTests = TestList [tSelfParent, tFirstParent, tSecondParent, 
                        tFirstGrandParent, tLongFamilyLine, tTwoAncestors]

tSelfParent :: Test
tSelfParent = "A^0" ~: revParseTree [Ancestry a [Parent 0]] getParentSet ~?=
      Set.fromList [a]

tFirstParent :: Test
tFirstParent = "A^1" ~: revParseTree [Ancestry a [Parent 1]] getParentSet ~?= 
      Set.fromList [b]

tSecondParent :: Test
tSecondParent = "A^2" ~: revParseTree [Ancestry a [Parent 2]] getParentSet ~?= 
      Set.fromList [c]

tFirstGrandParent :: Test
tFirstGrandParent = "A^^" ~: revParseTree [Ancestry a [Parent 1, Parent 1]] 
      getParentSet ~?= Set.fromList [d]

tLongFamilyLine :: Test
tLongFamilyLine = "A^^3^2" ~: revParseTree [Ancestry a [Parent 1, Parent 3, 
      Parent 2]] getParentSet ~?= Set.fromList [j]

tTwoAncestors :: Test
tTwoAncestors = "A~2" ~: revParseTree [Ancestry a [Ancestor 2]] getParentSet ~?=
      Set.fromList [d]

simpleRangeTests :: Test
simpleRangeTests = TestList [tSimpleRange1, tExcludeG, tExcludeD, tExcludeD2, 
                             tBSymmetricDiffC, tEmptyDash, tCaretHead, 
                             tCaretExclamation, tCaretExclamationRange]

tSimpleRange1 :: Test
tSimpleRange1 = "D F" ~: revParseTree [RevId d, RevId f] getParentSet ~?=
        Set.fromList [g, h, i, j, d, f]

tExcludeG :: Test
tExcludeG = "^G D" ~: revParseTree [Exclude (RevId g), RevId d] getParentSet ~?=
        Set.fromList [h,d]

tExcludeD :: Test
tExcludeD = "^D B" ~: revParseTree [Exclude (RevId d), RevId b] getParentSet ~?=
        Set.fromList [i,j, e, f, b]

tExcludeD2 :: Test
tExcludeD2 = "^D B C" ~: revParseTree [Exclude (RevId d), RevId b, RevId c] 
        getParentSet ~?= Set.fromList [i,j,e,f,b,c]

tBSymmetricDiffC :: Test
tBSymmetricDiffC = "B...C" ~: revParseTree [XOR (RevId b) (RevId c)] 
        getParentSet ~?= Set.fromList [g,h,d,e,b,c]

tEmptyDash :: Test
tEmptyDash = "B^-" ~: revParseTree [Caret b (Dash 1)] getParentSet ~?=
        Set.fromList [e,i,j,f,b]

tCaretHead :: Test
tCaretHead = "C^@" ~: revParseTree [Caret c Head] getParentSet ~?= 
        Set.fromList [i,j,f]

tCaretExclamation :: Test
tCaretExclamation = "C^!" ~: revParseTree [Caret c Exclamation] getParentSet ~?=
        Set.fromList [c]

tCaretExclamationRange :: Test
tCaretExclamationRange = "F^! D" ~: revParseTree [Caret f Exclamation, RevId d] 
        getParentSet ~?= Set.fromList [g,h,d,f]

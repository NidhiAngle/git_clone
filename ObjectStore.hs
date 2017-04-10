{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}
module ObjectStore where
import Crypto.Hash
import Data.ByteString.Char8 as C
import qualified Objects as O
import Text.Printf (printf)
import System.FilePath

type FileName = String
type Repo = String

getObjPath :: Repo -> O.ObjectId -> (FilePath, FileName)
getObjPath [] = ("","")
getObjPath o = "objects/" </> (take 2 o) </> (drop 2 o)


hashContent :: O.ObjectType -> C.ByteString -> (O.ObjectId, C.ByteString)
hashContent objType content = do
  let header           = getHeader (C.pack (show objType)) 
      headerAndContent = header `C.append` content
      id               = hexSha3_512 headerAndContent
  (id, headerAndContent) 
  where 
      getHeader objType  = objType `C.append` (C.pack " ") `C.append` (C.pack (show (C.length content))) `C.append` (C.pack "\0")



hexSha3_512 :: ByteString -> String
hexSha3_512 bs = show (hash bs :: Digest SHA3_512)





hi = Prelude.putStrLn "hi"
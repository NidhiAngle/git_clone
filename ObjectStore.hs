{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}
module ObjectStore where
import Crypto.Hash.SHA1 as SHA1
import Data.ByteString.Char8 as C
import qualified Objects as O
import Text.Printf (printf)

hashContent :: O.ObjectType -> C.ByteString -> (O.ObjectId, C.ByteString)
hashContent objType content = do
  let header           = getHeader (C.pack (show objType)) 
      headerAndContent = header `C.append` content
      id               = digestToString headerAndContent
  (id, headerAndContent) 
  where 
      getHeader objType  = objType `C.append` (C.pack " ") `C.append` (C.pack (show (C.length content))) `C.append` (C.pack "\0")

digestToString:: C.ByteString -> String
digestToString s = C.unpack (SHA1.hash s ) >>= printf "%02x" 

encodeObject :: O.ObjectType -> C.ByteString -> (O.ObjectId, C.ByteString)
encodeObject objectType content = do
    let header       = headerForBlob (C.pack $ show objectType)
        blob         = header `C.append` content
        sha1         = hsh blob
    (sha1, blob)
    where headerForBlob objType = objType `C.append` (C.pack " ") `C.append` C.pack (show $ C.length content) `C.append` (C.pack "\0")
          hsh = toHex . SHA1.hash

toHex :: C.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"


yo = Prelude.putStrLn "hi"
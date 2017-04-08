import Crypto.Hash.SHA1 as SHA1
import Data.ByteString.Char8 as C

hashContent :: ObjectType -> C.ByteString -> (ObjectId, C.Bytestring)
hashContent objType content = do
	let	header = getHeader (C.pack (show objType)) (C.pack (show $ C.length content))
	    headerAndContent = header `C.append` content
	    id = (toHex . SHA1.hash) headerandContent
	(id, headerAndContent) 
	where 
		getHeader objType length = objType `C.append` " " `C.append` length `C.append` "\0"
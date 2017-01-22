{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson
import Data.Bson
import Data.List.Utils as DLU
import Data.Text as T
import Data.Text.Lazy as TL
import Database.MongoDB as DDB
import GHC.Generics
import Prelude ()
import Prelude.Compat as PC
import Servant

-- This will be returned on a post request
data ApiResponse = ApiResponse {
  result :: Bool,
  message :: String
} deriving Generic

instance FromJSON ApiResponse
instance ToJSON ApiResponse

-- This will be returned on a request for a fileindex
data FileResponse = FileResponse {
  found :: Bool,
  fileIndex :: DirectoryDesc
} deriving Generic

instance FromJSON FileResponse
instance ToJSON FileResponse


-- Object that's stored in the database
data DirectoryDesc = DirectoryDesc {
  dbID :: String,
  fName :: String,
  fLocation :: String,
  fileServer :: String,
  port :: Int
} deriving Generic

instance FromJSON DirectoryDesc
instance ToJSON DirectoryDesc

fileIndexToDoc :: DirectoryDesc -> Document
fileIndexToDoc fi = ["name" =: (fName fi), "path" =: (fLocation fi), "server" =: (fileServer fi), "port" =: (port fi), "localID" =: (dbID fi)]

docToFileIndex :: Document -> DirectoryDesc
docToFileIndex doc = DirectoryDesc fid (unescape fname) (unescape fpath) (unescape fserver) portNo
  where
    fid = show $ DDB.valueAt "_id" doc
    fname = show $ DDB.valueAt "name" doc
    fpath = (show $ DDB.valueAt "path" doc) ++ "/"
    fserver = (show $ DDB.valueAt "server" doc)
    portNo = read $ show $ DDB.valueAt "port" doc

data UpdateObject = UpdateObject {
  old :: DirectoryDesc,
  new :: DirectoryDesc
} deriving Generic

instance FromJSON UpdateObject

unescape :: String -> String
unescape s = DLU.replace "\\" "" $ DLU.replace "\"" "" $ DLU.replace "\\\\" "" s

resolveFileIndex :: DirectoryDesc -> T.Text
resolveFileIndex fi = T.pack $ unescape (fLocation fi ++ fName fi)

-- Handle deleting files from the fileserver
data ObjIdentifier = ObjIdentifier {
  filePath :: String
} deriving Generic

instance FromJSON ObjIdentifier
instance ToJSON ObjIdentifier

-- A Data type that has the file path and id
data FileSummary = FileSummary {
  fileId :: String,
  fullPath :: String
} deriving Generic

instance ToJSON FileSummary

dirDescToObjId :: DirectoryDesc -> ObjIdentifier
dirDescToObjId fi = ObjIdentifier $ unescape (show (resolveFileIndex fi))

insertFile :: Document -> Action IO ()
insertFile newFile = insert_ "files" newFile

-- Take list of strings, return objidentifiers
stringToObj :: [String] -> [ObjIdentifier]
stringToObj [] = []
stringToObj x = (ObjIdentifier (PC.head x)) : stringToObj (PC.tail x)

drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if batch == []
        then return res
        else drainCursor' cur (res ++ batch)

-- Drain a cursor into ObjIdentifiers
docToObjs :: [Document] -> [ObjIdentifier]
docToObjs docs = PC.map dirDescToObjId fileIndexes
  where
    fileIndexes = PC.map docToFileIndex docs

fileIndexToSummary :: DirectoryDesc -> FileSummary
fileIndexToSummary fi = FileSummary {fileId=fid, fullPath=p}
  where
    fid = dbID fi
    p = (fLocation fi) ++ "/" ++ (fName fi)

type API = "new" :> ReqBody '[JSON] [DirectoryDesc] :> Post '[JSON] ApiResponse
         :<|> "update" :> ReqBody '[JSON] UpdateObject :> Put '[JSON] ApiResponse
         :<|> "resolve" :> ReqBody '[JSON] String :> Post '[JSON] (Either ApiResponse DirectoryDesc)
         :<|> "list" :> Get '[JSON] [FileSummary]

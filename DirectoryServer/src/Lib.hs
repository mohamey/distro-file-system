{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson
import Data.Bson
import Data.List.Utils as DLU
import Data.Text
import Database.MongoDB as DDB
import GHC.Generics
import Prelude ()
import Prelude.Compat as PC
import Servant

-- This is returned after some API queries
data ApiResponse = ApiResponse {
  result :: Bool,
  message :: String
} deriving Generic

instance FromJSON ApiResponse
instance ToJSON ApiResponse

-- This is a representation of each file on a file server that
-- will be stored in the directory server database
data DirectoryDesc = DirectoryDesc {
  dbID :: String,
  fName :: String,
  fLocation :: String,
  fileServer :: String,
  port :: Int
} deriving Generic

instance FromJSON DirectoryDesc
instance ToJSON DirectoryDesc

-- This takes a DirectoryDesc object and converts it to a Document
-- to be stored in the database
dirDescToDoc :: DirectoryDesc -> Document
dirDescToDoc fi = [
      "name" =: (fName fi),
      "path" =: (fLocation fi),
      "server" =: (fileServer fi),
      "port" =: (port fi),
      "localID" =: (dbID fi)
  ]

-- This converts a database document into a DirectoryDesc record
docToDirDesc :: Text -> Document -> DirectoryDesc
docToDirDesc idCol doc = DirectoryDesc fid (unescape fname) (unescape fpath) (unescape fserver) portNo
  where
    fid = show $ DDB.valueAt idCol doc
    fname = show $ DDB.valueAt "name" doc
    fpath = (show $ DDB.valueAt "path" doc) ++ "/"
    fserver = (show $ DDB.valueAt "server" doc)
    portNo = read $ show $ DDB.valueAt "port" doc

-- This is sent by the client and handles moving an object
data UpdateObject = UpdateObject {
  old :: DirectoryDesc,
  new :: DirectoryDesc
} deriving Generic

instance FromJSON UpdateObject

-- This unescapes strings read from the database
unescape :: String -> String
unescape s = DLU.replace "\\" "" $ DLU.replace "\"" "" $ DLU.replace "\\\\" "" s

-- A Data type that has the file path and id
data FileSummary = FileSummary {
  fileId :: String,
  fullPath :: String
} deriving Generic

instance ToJSON FileSummary


drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if batch == []
        then return res
        else drainCursor' cur (res ++ batch)

dirDescToSummary :: DirectoryDesc -> FileSummary
dirDescToSummary fi = FileSummary {fileId=fid, fullPath=p}
  where
    fid = dbID fi
    p = (fLocation fi) ++ (fName fi)

type API = "new" :> ReqBody '[JSON] [DirectoryDesc] :> Post '[JSON] ApiResponse
         :<|> "update" :> ReqBody '[JSON] UpdateObject :> Put '[JSON] ApiResponse
         :<|> "resolve" :> ReqBody '[JSON] String :> Post '[JSON] (Either ApiResponse DirectoryDesc)
         :<|> "list" :> Get '[JSON] [FileSummary]

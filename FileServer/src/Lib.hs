{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Data.Aeson
import Data.Bson
import Data.List.Utils as DLU
import Data.Text as T
import Data.Time.Clock
import qualified Data.Text.Lazy as TL
import Database.MongoDB as DDB
import GHC.Generics
import Prelude ()
import Prelude.Compat as PC
import Servant

type FileRequest = Either ApiResponse FileObject
type DescRequest = Either ApiResponse DirectoryDesc
type DescsRequest = Either ApiResponse [DirectoryDesc]

-- A representation of files on the server
-- It contains the file path and its contents
data FileObject = FileObject {
  path :: String,
  fileContent :: TL.Text,
  modifiedLast :: UTCTime
} deriving Generic

instance FromJSON FileObject
instance ToJSON FileObject

-- This will be returned on a post request
data ApiResponse = ApiResponse {
  result :: Bool,
  message :: String
} deriving Generic

instance FromJSON ApiResponse
instance ToJSON ApiResponse

-- Object that's stored in the database
data FileIndex = FileIndex {
  fileName :: String,
  fileLocation :: String,
  lastModified :: UTCTime
} deriving Generic

-- A Record representing how file details are stored
-- on the directory server
data DirectoryDesc = DirectoryDesc {
  dbID :: String,
  fName :: String,
  fLocation :: String,
  fileServer :: String,
  port :: Int,
  modified :: UTCTime
} deriving Generic

instance FromJSON DirectoryDesc
instance ToJSON DirectoryDesc

-- A Data type that has the file path and id
-- It is a trimmed down identifier of files listed
-- in directory server
data FileSummary = FileSummary {
  fileId :: String,
  fullPath :: String
} deriving Generic

instance FromJSON FileSummary
instance ToJSON FileSummary

-- This object is sent to carry out a move file operation
-- It details the files previous location and then it's new location
data UpdateObject = UpdateObject {
  old :: DirectoryDesc,
  new :: DirectoryDesc
} deriving Generic

instance FromJSON UpdateObject
instance ToJSON UpdateObject

-- Handle deleting files from the fileserver
data ObjIdentifier = ObjIdentifier {
  filePath :: String
} deriving Generic

instance FromJSON ObjIdentifier
instance ToJSON ObjIdentifier


fileIndexToDoc :: FileIndex -> Document
fileIndexToDoc (FileIndex {fileName=fn, fileLocation=fl, lastModified=t}) = ["name" =: fn, "path" =: fl, "modified"=:t]

unescape :: String -> String
unescape s = DLU.replace "\\" "" $ DLU.replace "\"" "" $ DLU.replace "\\\\" "" s

docToFileIndex :: Document -> FileIndex
docToFileIndex doc = FileIndex (unescape fname) (unescape fpath) time
  where
    fname = show $ DDB.valueAt "name" doc 
    fpath = (show $ DDB.valueAt "path" doc) ++ "/"
    time = read (show $ DDB.valueAt "modified" doc) :: UTCTime

resolveFileIndex :: FileIndex -> T.Text
resolveFileIndex fi = T.pack $ unescape (fileLocation fi ++ fileName fi)

fileIndexToObjId :: FileIndex -> ObjIdentifier
fileIndexToObjId fi = ObjIdentifier $ unescape (show (resolveFileIndex fi))

insertFile :: Document -> Action IO ()
insertFile newFile = insert_ "files" newFile

-- Take list of strings, return objidentifiers
stringToObj :: [String] -> [ObjIdentifier]
stringToObj [] = []
stringToObj x = (ObjIdentifier (PC.head x)) : stringToObj (PC.tail x)

concatStrings :: [[Char]] -> [[Char]] -> [[Char]]
concatStrings [] _ = []
concatStrings _ [] = []
concatStrings x y = ((PC.head x) ++ (PC.head y)) : concatStrings (PC.tail x) (PC.tail y)

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
docToObjs docs = PC.map fileIndexToObjId fileIndexes
  where
    fileIndexes = PC.map docToFileIndex docs

docToDirDesc :: String -> Int -> Document -> DirectoryDesc
docToDirDesc ip portNo doc = DirectoryDesc (unescape fid) (unescape fn) (unescape fl) ip portNo timeStamp
  where
    fid = show $ valueAt "_id" doc
    fn = show $ valueAt "name" doc
    fl = show $ valueAt "path" doc
    timeStamp = read (show $ valueAt "modified" doc) :: UTCTime

data FileServer = FileServer {
  address :: String,
  portNum :: Int
} deriving Generic

instance ToJSON FileServer
instance FromJSON FileServer

-- The API Definition
type API = "upload" :> ReqBody '[JSON] FileObject :> Post '[JSON] ApiResponse
         :<|> "remove" :> ReqBody '[JSON] ObjIdentifier :> Delete '[JSON] ApiResponse
         :<|> "removeSec" :> ReqBody '[JSON] ObjIdentifier :> Delete '[JSON] ApiResponse
         :<|> "update" :> ReqBody '[JSON] FileObject :> Put '[JSON] ApiResponse
         :<|> "close" :> ReqBody '[JSON] FileObject :> Put '[JSON] ApiResponse
         :<|> "open" :> QueryParam "path" String :> Get '[JSON] FileRequest
         :<|> "files" :> QueryParam "path" String :> Get '[JSON] FileRequest
         :<|> "list" :> Get '[JSON] [ObjIdentifier]

type FSAPI = "update" :> ReqBody '[JSON] FileObject :> Put '[JSON] ApiResponse
         :<|> "removeSec" :> ReqBody '[JSON] ObjIdentifier :> Delete '[JSON] ApiResponse

type DSAPI = "new" :> ReqBody '[JSON] [DirectoryDesc] :> Post '[JSON] ApiResponse
         :<|> "update" :> ReqBody '[JSON] UpdateObject :> Put '[JSON] ApiResponse
         :<|> "add" :> ReqBody '[JSON] FileServer :> Post '[JSON] ApiResponse
         :<|> "delete" :> ReqBody '[JSON] DirectoryDesc :> Delete '[JSON] ApiResponse
         :<|> "getSecondaries" :> QueryParam "path" String :> Get '[JSON] DescsRequest

splitFullPath :: TL.Text -> (TL.Text, TL.Text)
splitFullPath p = (filePth, fileNme)
  where
    parts = TL.splitOn "/" p
    filePth = TL.intercalate "/" (PC.init parts)
    fileNme = PC.last parts

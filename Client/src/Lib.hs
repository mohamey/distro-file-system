{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Data.Aeson
import Data.Bson
import Data.Time.Clock
import Data.Text as T
import Database.MongoDB
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Prelude ()
import Prelude.Compat as PC
import Servant
import System.Console.ANSI

-- A representation of files on the server
-- This will be stored in the mongodb database
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
  fileName :: T.Text,
  fileLocation :: T.Text,
  lastModified :: UTCTime
} deriving Generic

fileIndexToDoc :: FileIndex -> Document
fileIndexToDoc (FileIndex {fileName=fn, fileLocation=fl, lastModified=t}) = ["name" =: fn, "path" =: fl, "modified" =: t]

insertFile :: Document -> Action IO ()
insertFile newFile = insert_ "files" newFile

-- Handle deleting files from the fileserver
data ObjIdentifier = ObjIdentifier {
  filePath :: String
} deriving Generic

instance FromJSON ObjIdentifier
instance ToJSON ObjIdentifier

-- This is a representation of each file on a file server that
-- will be stored in the directory server database
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

-- This is sent by the client and handles moving an object
data UpdateObject = UpdateObject {
  old :: DirectoryDesc,
  new :: DirectoryDesc
} deriving Generic

instance FromJSON UpdateObject
instance ToJSON UpdateObject

-- A Data type that has the file path and id
data FileSummary = FileSummary {
  fileId :: String,
  fullPath :: String
} deriving Generic

instance ToJSON FileSummary
instance FromJSON FileSummary

-- A representation of a file server in the directory server
data FileServer = FileServer {
  address :: String,
  portNum :: Int
} deriving Generic

instance FromJSON FileServer
instance ToJSON FileServer

data ResolveRequest = ResolveRequest {
  requestId :: String,
  prim :: Bool
} deriving Generic

instance FromJSON ResolveRequest
instance ToJSON ResolveRequest

-- The API Definition
type API = "upload" :> ReqBody '[JSON] FileObject :> Post '[JSON] ApiResponse
         :<|> "remove" :> ReqBody '[JSON] ObjIdentifier :> Delete '[JSON] ApiResponse
         :<|> "update" :> ReqBody '[JSON] FileObject :> Put '[JSON] ApiResponse
         :<|> "close" :> ReqBody '[JSON] FileObject :> Put '[JSON] ApiResponse
         :<|> "open" :> QueryParam "path" String :> Get '[JSON] (Either ApiResponse FileObject)
         :<|> "files" :> QueryParam "path" String :> Get '[JSON] FileObject
         :<|> "list" :> Get '[JSON] [ObjIdentifier]

type DSAPI ="resolve" :> ReqBody '[JSON] ResolveRequest :> Post '[JSON] (Either ApiResponse DirectoryDesc)
         :<|> "list" :> Get '[JSON] [FileSummary]
         :<|> "getFs" :> Get '[JSON] FileServer


-- Colorful output!
redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
greenCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Green]
resetCode = setSGRCode [Reset]

splitFullPath :: TL.Text -> (TL.Text, TL.Text)
splitFullPath p = (filePth, fileNme)
  where
    parts = TL.splitOn "/" p
    filePth = TL.intercalate "/" (PC.init parts)
    fileNme = PC.last parts


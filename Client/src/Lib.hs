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
import Data.Text as T
import Database.MongoDB
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Prelude ()
import Prelude.Compat as PC
import Servant

-- A representation of files on the server
-- This will be stored in the mongodb database
data FileObject = FileObject {
  path :: String,
  fileContent :: TL.Text
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
  fileLocation :: T.Text
} deriving Generic

fileIndexToDoc :: FileIndex -> Document
fileIndexToDoc (FileIndex {fileName=fn, fileLocation=fl}) = ["name" =: fn, "path" =: fl]

insertFile :: Document -> Action IO ()
insertFile newFile = insert_ "files" newFile

-- Handle deleting files from the fileserver
data ObjIdentifier = ObjIdentifier {
  filePath :: String
} deriving Generic

instance FromJSON ObjIdentifier
instance ToJSON ObjIdentifier

-- The API Definition
type API = "upload" :> ReqBody '[JSON] FileObject :> Post '[JSON] ApiResponse
         :<|> "remove" :> ReqBody '[JSON] ObjIdentifier :> Delete '[JSON] ApiResponse
         :<|> "update" :> ReqBody '[JSON] FileObject :> Put '[JSON] ApiResponse
         :<|> "files" :> QueryParam "path" String :> Get '[JSON] FileObject
         :<|> "list" :> Get '[JSON] [ObjIdentifier]
